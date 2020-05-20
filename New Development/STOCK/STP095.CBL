      $set LINKCOUNT"468" GNT"STP095.GNT"
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
      *    ******   ********  *******     ****     ******   ********   *
      *   **    **     **     **    **   **  **   **    **  **         *
      *   **           **     **    **  **    **  **    **  *******    *
      *    ******      **     *******   **    **   *******        **   *
      *         **     **     **        **    **        **        **   *
      *   **    **     **     **         **  **   **    **  **    **   *
      *    ******      **     **          ****     ******    ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *   D E L E T E   S T O C K   R A N G E ( FLAG 9 TO STK-DISC)  *
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
       PROGRAM-ID.     STP095.
       AUTHOR.         J.W.LEMMON.
       DATE-WRITTEN.   AUGUST 1989.

                COPYRIGHT NOTICE: COPYRIGHT (C) 1989 - 2018
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

       COPY "DEPART.SL".

       COPY "PARAM.SL".

       COPY "SPARTS.SL".

       COPY "STKMEM.SL".

       COPY "STOCK.SL".

       COPY "WSTOCK.SL".

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

       COPY "DEPART.FDE".

       COPY "PARAM.FDE".

       COPY "SPARTS.FDE".

       COPY "STKMEM.FDE".

       COPY "STOCK.FDE".

       COPY "WSTOCK.FDE".

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
       77  WS-DISC    PIC 999V99  COMP-3.
       77  WS-SUB    PIC 9(04)  COMP-5.
       77  WS-S1    PIC 9(04)  COMP-5.
       77  WS-S2    PIC 9(04)  COMP-5.
       77  WS-S3    PIC 9(04)  COMP-5.
       77  WS-S4    PIC 9(04)  COMP-5.
       77  WS-S5    PIC 9(04)  COMP-5.
       77  WS-S6    PIC 9(04)  COMP-5.
       77  WS-S7    PIC 9(04)  COMP-5.
       77  WS-S8    PIC 9(04)  COMP-5.
       77  WS-IXS    PIC 9(04)  COMP-5.
       77  WS-IXS1    PIC 9(04)  COMP-5.
       77  WS-IXS2    PIC 9(04)  COMP-5.
       77  WS-IXS3    PIC 9(04)  COMP-5.
       77  WS-IXS4    PIC 9(04)  COMP-5.
       77  WS-TRFKEY    PIC 9(06)  COMP-5.
       77  WS-PRCKEY    PIC 9(06)  COMP-5.
       77  WS-PARKEY    PIC 9(06)  COMP-5.
       77  WS-NETKEY    PIC 9(06)  COMP-5.
       77  WS-RECKEY    PIC 9(06)  COMP-5.
       77  WS-RECOVER    PIC 9(06)  COMP-5.
       77  WS-RECORDS    PIC 9(06)  COMP-5.
       77  WS-TRANS    PIC 9(04)  COMP-5 VALUE 1.
       77  WS-KEYSTR    PIC 9(04)  COMP-5.
       77  WS-MONTHS    PIC S9(01)V99 COMP-3.
       77  WS-KEY1    PIC 9(04)  COMP-5.
       77  WS-ITM    PIC X(18).
       77  WS-ETYPE    PIC X(01).
       77  WS-PAGE    PIC 9(04)  COMP-5.
       77  WS-NORM         PIC  X(04).
       77  WS-COND         PIC  X(04).
       77  WS-EXPP         PIC  X(04).
       77  WS-ECAN         PIC  X(04).
       77  WS-10CPI        PIC  X(04).
       77  WS-12CPI        PIC  X(04).
       77  WS-17CPI        PIC  X(04).
       77  WS-OPTION    PIC X(01).
       77  WS-SKIP    PIC X(01).
       77  WS-ADJ    PIC X(01).
       77  WS-ERROR    PIC 9(01).
       77  WS-AMEND    PIC 9(01)  VALUE 0.
       77  WS-CONREC    PIC 9(01) VALUE ZERO.
       77  WS-WH-QUANT    PIC S9(08)V999 COMP-3.
       77  WS-LEDG    PIC X(04).
       77  WS-LEDG1    PIC X(04).
       77  WS-USE-ITM    PIC X(01).
       77  WS-EXT-STK    PIC X(01).
       77  WS-COMP    PIC 9(01).
       77  WS-WEEK    PIC 9(01).
       77  WS-TYPE    PIC X(01).
       77  WS-BATCH    PIC 9(05).
       77  WS-PASSWORD    PIC X(08).
       77  WS-PASS    PIC X(08).
       77  WS-DESC-H1    PIC X(14) VALUE "Description".
       77  WS-DESC-H2    PIC X(14) VALUE "Description 2".
       77  WS-RT-HD    PIC X(10) VALUE "-Retail   ".
       77  WS-WS-HD    PIC X(10) VALUE "-Wholesale".
       77  WS-CS-HD    PIC X(10) VALUE "-Cash sale".
       77  WS-NODEP    PIC X(30) VALUE
      "NO SUCH   D E P A R T M E N T".
       77  PRG-NAME    PIC X(12) VALUE "STP\STPLOOK".
       77  TODAY-DDMMYY    PIC 9(08) COMP-5.
       77  WS-USUB    PIC 9(04) COMP-5.

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
           03  CRT-PROGRAM         PIC  X(15) VALUE "STP095".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "STOCK/INVENTORY -DELETE A STOCK RANGE".
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
    03  WS-MODULE       PIC  X(03) VALUE "STP".
    03  WS-PROG        PIC  X(03) VALUE "095".

       01  WS-PARID.
          03  WS-SYS-ID       PIC  X(03).

       COPY "W05.VAT".

       COPY "W05.RND".

       01  W09-STOCK.
           03  W09-MARKUP          PIC S9(03)V9999 COMP-3.
           03  W09-CMARKUP         PIC S9(03)V9999 COMP-3.
           03  W09-WMARKUP         PIC S9(03)V9999 COMP-3.

       COPY "W10.STK".

       COPY "W11.STK".

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

       COPY "W15.STK".

       COPY "W25.STK".

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       COPY "W75.STK".

       01  W80-EDIT.
           03  W80-CNO        PIC 9(04).
           03  W80-V1.
               05  W80-S7V2   PIC Z(06)9.99-.
           03  W80-V2.
               05  W80-S5V2   PIC Z(04)9.99-.
           03  W80-DTE.
              05  W80-DATE   PIC Z9/99/9999.
           03  W80-QNT.
               05  W80-5      PIC Z(04)9.99.
           03  W80-QNT1 REDEFINES W80-QNT.
               05  W80-3V2    PIC Z(02)9.9999.
           03  W80-NO.
               05  W80-5N     PIC 9(05).
               05  W80-REC REDEFINES W80-5N
                              PIC ZZZZ9.
           03  W80-DEL.
               05  W80-2N      PIC 9(02).
       01  W85-PASS.
           03  W85-SUPER       PIC  X(06)    OCCURS 9.
           03  W85-ENTRY       PIC  9(02)    COMP-3.
           03  W85-MARG        PIC S9(02)V99.
       01  W90-BAL             PIC S9(07)V99 COMP-3.
       01  W95-COMP            PIC X(40).
       01  W100-EDIT.
           03  W100-V2.
               05  W100-S6V2   PIC Z(05)9.99-.
           03  W100-V3 REDEFINES W100-V2.
               05  W100-PRICE  PIC X(09).
               05  FILLER      PIC X(01).
           03  W100-V5 REDEFINES W100-V2.
               05  W100-QNT    PIC Z(05)9.99-.

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

       01 S05-ITEM.
   03  LINE 12 COLUMN 23 FOREGROUND-COLOR Cyan HIGHLIGHT
    BACKGROUND-COLOR Magenta
    PIC  X(14) USING W10-ITEM.

       01 S05-ITM.
   03  LINE 12 COLUMN 23 FOREGROUND-COLOR Cyan HIGHLIGHT
    BACKGROUND-COLOR Magenta
    PIC  X(14) USING W10-ITEM.
   03       COLUMN 37 FOREGROUND-COLOR Black
    BACKGROUND-COLOR Magenta
    VALUE "/".
   03       COLUMN 38 FOREGROUND-COLOR Cyan HIGHLIGHT
    BACKGROUND-COLOR Magenta
    PIC  X(03) USING W10-ITM.

       01 S05-EXT-ITEM.
   03  LINE 12 COLUMN 23 FOREGROUND-COLOR Cyan HIGHLIGHT
    BACKGROUND-COLOR Magenta
    PIC  X(18) USING W10-EXT-ITEM.

       01 S06-ITEM.
   03  LINE 13 COLUMN 23 FOREGROUND-COLOR Cyan HIGHLIGHT
    BACKGROUND-COLOR Magenta
    PIC  X(14) USING W10-ITEM.

       01 S06-ITM.
   03  LINE 13 COLUMN 23 FOREGROUND-COLOR Cyan HIGHLIGHT
    BACKGROUND-COLOR Magenta
    PIC  X(14) USING W10-ITEM.
   03       COLUMN 37 FOREGROUND-COLOR Black
    BACKGROUND-COLOR Magenta
    VALUE "/".
   03       COLUMN 38 FOREGROUND-COLOR Cyan HIGHLIGHT
    BACKGROUND-COLOR Magenta
    PIC  X(03) USING W10-ITM.

       01 S06-EXT-ITEM.
   03  LINE 13 COLUMN 23 FOREGROUND-COLOR Cyan HIGHLIGHT
    BACKGROUND-COLOR Magenta
    PIC  X(18) USING W10-EXT-ITEM.

       01  S40.
    03  FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta
    PIC X(04) USING WS-LEDG AUTO.

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
      *
      *   **** Check the User's security level for Stock/Inventory
      *
          IF LS0-STLEV < 9
      *
      *      Insufficient security level
      *
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
        GO TO AA49.
            PERFORM ZA000-INIT.
            PERFORM BZ000.

       AA49.
      EXIT PROGRAM.

       COPY "AA50.LUP".

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

       COPY "ABORT.PRN".

       COPY "AA300.STK".

      *
      *    ****    R E A D   F I L E S   R O U T I N E S
      *
       AC000-READ              SECTION.

       COPY "CONTROL.RD".

       COPY "DEPART.RD".

       COPY "PARAM.RD".

       COPY "STKMEM.RD".

       COPY "STOCK.RD".

       COPY "SPARTS.RD".

       COPY "WSTOCK.RD".

      *
      *   R E W R I T E   &    W R I T E   F I L E S   R O U T I N E S
      *
       AD000-WRITE             SECTION.

       COPY "CONTROL.WR".

       COPY "DEPART.WR".

       COPY "PARAM.WR".

       COPY "STKMEM.WR".

       COPY "STOCK.WR".

       COPY "SPARTS.WR".

       COPY "WSTOCK.WR".

       COPY "STOCK.1ST".

       COPY "APAC.HLP".

       COPY "CA000.STK".

       COPY "CA100.STK".

       COPY "CA200.STK".

       COPY "CA500.STK".

       COPY "ROUND.PRC".

      /
      *    ****    D E L E T E   A   S T O C K   R A N G E
      *
       BZ000        SECTION 7.
       BZ00.
             PERFORM ERASE-SCREEN.
             DISPLAY "DELETE A RANGE OF STOCK ITEMS" AT 0226
                      WITH FOREGROUND-COLOR Grey HIGHLIGHT.
             DISPLAY "Run deletion routine (Y/N)" AT 0404.
       BZ02.
             ACCEPT WS-OPTION AT 0431 WITH FOREGROUND-COLOR Grey HIGHLIGHT AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
          IF NOT (WS-OPTION = "N" OR "Y")
               GO TO BZ02.
           IF WS-OPTION = "N"
              GO TO BZ999.
             MOVE 5              TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-PW         TO W85-PASS.
             DISPLAY "Enter password >      <" AT 5051
              WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT.
            ACCEPT W40-CSHPASS AT 5067
      WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Grey HIGHLIGHT
          AUTO NO-ECHO.
            MOVE 6   TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING W40-CSHPASS
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
            MOVE 1   TO WS-LENGTH.
           IF NOT (W40-CSHPASS = W85-SUPER(1)  OR
                                 W85-SUPER (7) OR
                                 "ZERODP")
               GO TO BZ999.
             DISPLAY "                       " AT 5051
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Cyan.

       BZ02A.
            MOVE SPACES  TO WS-LEDG.
            DISPLAY "Sales ledger Code" AT 1004.
             DISPLAY "Press 'ENTER' to exit" AT 2304.

       BZ03.
            ACCEPT S40 AT 1022.
            DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR Cyan.
          IF WS-LEDG = SPACES
               GO TO BZ999.
            MOVE WS-LEDG  TO DPT-CODE.
    IF (DPT-CODE = "XXXX" OR "YYYY") OR
       (DPT-SCDE = "VAT")
              MOVE "Invalid ledger number"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO BZ03.
             PERFORM READ-DEPART THRU READ-DEPART-EXIT.
    IF WS-F-ERROR = 7
              MOVE "No record"  TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO BZ03.
             DISPLAY DPT-DESC AT 1045 WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
      PERFORM CHECK-CORRECT.
    IF WS-OPTION = "N"
               GO TO BZ03.
      DISPLAY "Item Code - Start: " AT 1204
       "                  "
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Magenta.
      DISPLAY "- End  : " AT 1314
       "                  "
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Magenta.
    IF WS-USE-ITM = "Y"
        DISPLAY "/" AT 1237 WITH FOREGROUND-COLOR Black
     BACKGROUND-COLOR Magenta
        DISPLAY "/" AT 1337 WITH FOREGROUND-COLOR Black
     BACKGROUND-COLOR Magenta.

       BZ05.
      PERFORM AA300.
    IF WS-USE-ITM = "Y"
              ACCEPT S05-ITM
    ELSE
    IF WS-EXT-STK = "Y"
        ACCEPT S05-EXT-ITEM
    ELSE
        ACCEPT S05-ITEM.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE SPACES TO W10-EXT-ITEM
       GO TO BZ999
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
       GO TO BZ05
   WHEN F2-KEY
       MOVE "I"  TO W10-ETYPE
   WHEN F3-KEY
       MOVE "A"  TO W10-ETYPE
   WHEN F4-KEY
       MOVE "D"  TO W10-ETYPE
   WHEN F5-KEY
       MOVE "2"  TO W10-ETYPE
   WHEN F6-KEY
       MOVE "X"  TO W10-ETYPE
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO BZ05
        END-EVALUATE
        PERFORM AA50-LOOKUP
        IF WS-EXT-STK = "Y"
     DISPLAY S05-EXT-ITEM
        ELSE
        IF WS-USE-ITM = "Y"
     DISPLAY S05-ITM
        ELSE
     DISPLAY S05-ITEM
        END-IF
        IF W10-EXT-ITEM = SPACES
                   GO TO BZ05.
      DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR Cyan.
      PERFORM CLEAR-L50.
          IF W10-EXT-ITEM = SPACES
               GO TO BZ999.

       BZ10.
             PERFORM CA000.
           IF WS-F-ERROR = 22
        MOVE "Z"   TO W10-ETYPE
              PERFORM AA50-LOOKUP
        IF WS-EXT-STK = "Y"
     DISPLAY S05-EXT-ITEM
        ELSE
        IF WS-USE-ITM = "Y"
     DISPLAY S05-ITM
        ELSE
     DISPLAY S05-ITEM
        END-IF
              IF W10-EXT-ITEM = SPACES
                   GO TO BZ999
               ELSE
                   GO TO BZ10.
             DISPLAY STK-DESC AT 1239 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
      PERFORM CHECK-CORRECT.
    IF WS-OPTION = "N"
               GO TO BZ05.
      MOVE W10-EXT-ITEM  TO WS-ITM.

       BZ15.
             PERFORM AA300.
    IF WS-USE-ITM = "Y"
              ACCEPT S06-ITM
    ELSE
    IF WS-EXT-STK = "Y"
        ACCEPT S06-EXT-ITEM
    ELSE
        ACCEPT S06-ITEM.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE SPACES TO W10-EXT-ITEM
       GO TO BZ999
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
       GO TO BZ15
   WHEN F2-KEY
       MOVE "I"  TO W10-ETYPE
   WHEN F3-KEY
       MOVE "A"  TO W10-ETYPE
   WHEN F4-KEY
       MOVE "D"  TO W10-ETYPE
   WHEN F5-KEY
       MOVE "2"  TO W10-ETYPE
   WHEN F6-KEY
       MOVE "X"  TO W10-ETYPE
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO BZ15
        END-EVALUATE
        PERFORM AA50-LOOKUP
        IF WS-EXT-STK = "Y"
     DISPLAY S06-EXT-ITEM
        ELSE
        IF WS-USE-ITM = "Y"
     DISPLAY S06-ITM
        ELSE
     DISPLAY S06-ITEM
        END-IF
        IF W10-EXT-ITEM = SPACES
                   GO TO BZ15.
      DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR Cyan.
      PERFORM CLEAR-L50.
          IF W10-EXT-ITEM = SPACES
               GO TO BZ999.

       BZ20.
             PERFORM CA000.
           IF WS-F-ERROR = 22
        MOVE "Z"   TO W10-ETYPE
              PERFORM AA50-LOOKUP
        IF WS-EXT-STK = "Y"
     DISPLAY S06-EXT-ITEM
        ELSE
        IF WS-USE-ITM = "Y"
     DISPLAY S06-ITM
        ELSE
     DISPLAY S06-ITEM
        END-IF
              IF W10-EXT-ITEM = SPACES
                   GO TO BZ999
               ELSE
                   GO TO BZ20.
             DISPLAY STK-DESC AT 1339 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
      PERFORM CHECK-CORRECT.
    IF WS-OPTION = "N"
               GO TO BZ15.
      MOVE W10-EXT-ITEM  TO W10-EXT-CODE2.
             MOVE WS-ITM         TO STK-CODE.
             PERFORM START-AT-STOCK-CODE THRU READ-STOCK-EXIT.
      DISPLAY "Deleting :" AT 1504.

       BZ25.
             PERFORM READ-STOCK-NEXT-LOCK THRU READ-STOCK-EXIT.
    IF WS-F-ERROR = 22
        GO TO BZ90.
    IF STK-CODE > W10-EXT-CODE2
               UNLOCK STOCK
        GO TO BZ90.
    IF WS-LEDG NOT = STK-LDG
               UNLOCK STOCK
               GO TO BZ25.
    IF (STK-QUANT NOT = ZERO) OR
       (STK-ORD NOT = ZERO) OR
       (STK-QPIC NOT = ZERO) OR
       (STK-QJOB NOT = ZERO)
        UNLOCK STOCK
        GO TO BZ25.
      MOVE ZERO   TO WS-WH-QUANT.
      PERFORM BZ30 THRU BZ40.
    IF NOT (WS-WH-QUANT = ZERO)
        UNLOCK STOCK
        GO TO BZ25.
      DISPLAY STK-CODE AT 1515 WITH FOREGROUND-COLOR Grey HIGHLIGHT
                     " " STK-DESC WITH FOREGROUND-COLOR Grey HIGHLIGHT.
             MOVE STK-CODE       TO PRT-ITEM.
             PERFORM READ-SPARTS-LOCK THRU READ-SPARTS-EXIT.
           IF WS-F-ERROR = 0
              PERFORM DELETE-SPARTS-REC THRU WRITE-SPARTS-EXIT.
      PERFORM BZ45 THRU BZ55.
      MOVE 9   TO STK-DISC.
      PERFORM REWRITE-STOCK-UNLOCK THRU WRITE-STOCK-EXIT.
             PERFORM PRINT-ABORT.
           IF WS-OPTION = "Y"
               DISPLAY "Process aborted by operator - Press any key"
         AT 5002
         WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
        ACCEPT WS-OPTION AT 5046
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Grey HIGHLIGHT AUTO
               GO TO BZ999.
             GO TO BZ25.
      *
      *    ****    C H E C K   F O R   Q U A N T I T Y  I N
      *     W A R E H O U S E   R E C O R D S
      *
       BZ30.
      MOVE STK-CODE  TO WST-EXT-CODE.
      MOVE SPACES  TO WST-WAR.
      PERFORM START-AT-WH-ITEM THRU READ-WSTOCK-EXIT.
    IF WS-F-ERROR = 52
        GO TO BZ40.

       BZ35.
      PERFORM READ-WSTOCK-NEXT THRU READ-WSTOCK-EXIT.
    IF WS-F-ERROR = 52
        GO TO BZ40.
    IF NOT (STK-CODE = WST-EXT-CODE)
        GO TO BZ40.
      ADD WST-QUANT  TO WS-WH-QUANT.
      GO TO BZ35.

       BZ40.
      EXIT.
      *
      *    ****    D E L E T E  W A R E H O U S E   R E C O R D S
      *
       BZ45.
      MOVE STK-CODE  TO WST-EXT-CODE.
      MOVE SPACES  TO WST-WAR.
      PERFORM START-AT-WH-ITEM THRU READ-WSTOCK-EXIT.
    IF WS-F-ERROR = 52
        GO TO BZ55.

       BZ50.
      PERFORM READ-WSTOCK-NEXT-LOCK THRU READ-WSTOCK-EXIT.
    IF WS-F-ERROR = 52
        GO TO BZ55.
    IF NOT (STK-CODE = WST-EXT-CODE)
        GO TO BZ55.
      PERFORM DELETE-WSTOCK-REC THRU WRITE-WSTOCK-EXIT.
      GO TO BZ50.

       BZ55.
      EXIT.

       BZ90.
             DISPLAY "Range deleted" AT 1704 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             DISPLAY "Press any key to continue" AT 1904.
             ACCEPT WS-OPTION AT 1930 WITH AUTO.

       BZ999.
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
            MOVE 3   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      MOVE PAR-USE-ITM  TO WS-USE-ITM.
      MOVE PAR-EXT-STK  TO WS-EXT-STK.

       ZA01B.
             MOVE 1              TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-DMY        TO W12-TODAY.
             MOVE PAR-YMD        TO W12-T-YMD.
             MOVE PAR-COMPANY    TO W95-COMP.
             MOVE 3              TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
             MOVE STK-WEEK       TO WS-WEEK.
             MOVE STK-RECORDS    TO WS-RECORDS.
             MOVE STK-PASSWORD   TO WS-PASSWORD.
             MOVE 1              TO W05-V-RATE.

       ZA05-VAT.
             MOVE W05-VAT-CODE       TO DPT-CODE.
             PERFORM READ-DEPART THRU READ-DEPART-EXIT.
             MOVE DPT-R-DATE         TO WS-VAT-DATE(W05-V-RATE).
             MOVE DPT-RATE           TO W05-VAT(W05-V-RATE).
             ADD 6 W05-V-RATE        GIVING WS-S1.
             MOVE DPT-P-RATE         TO W05-VAT(WS-S1).
           IF W05-V-RATE < 6
               ADD 1                 TO W05-V-RATE
               GO TO ZA05-VAT.
             GO TO ZA999-EXIT.
       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
             ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR Grey HIGHLIGHT.
             GO TO ZA205.

       ZA200.

       COPY "LOCKED.PRO".

       ZA205.
             EXIT PROGRAM.

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
               WHEN 7     MOVE W02-DEPART   TO WS-FILE
                          MOVE DPT-CODE     TO WS-KEYX
               WHEN 15    MOVE WS-PARID     TO WS-FILE
                          MOVE WS-PARKEY    TO WS-KEY
               WHEN 21    MOVE W02-SPARTS   TO WS-FILE
                          MOVE PRT-ITEM     TO WS-KEYX
               WHEN 22    MOVE W02-STOCKF   TO WS-FILE
                          MOVE STK-CODE     TO WS-KEYX
               WHEN 25    MOVE W02-STKMEM   TO WS-FILE
                          MOVE SME-KEY      TO WS-KEYX
               WHEN OTHER MOVE "ERROR"      TO WS-FILE
                          MOVE "STP095"     TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
