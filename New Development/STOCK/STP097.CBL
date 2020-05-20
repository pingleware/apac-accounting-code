      $set LINKCOUNT"384" GNT"STP097.GNT"
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
      *    ******   ********  *******     ****     ******    ********  *
      *   **    **     **     **    **   **  **   **    **        **   *
      *   **           **     **    **  **    **  **    **       **    *
      *    ******      **     *******   **    **   *******      **     *
      *         **     **     **        **    **        **     **      *
      *   **    **     **     **         **  **   **    **     **      *
      *    ******      **     **          ****     ******      **      *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *   S T O C K   P R I C E   A D J U S T M E N T   P R O G R A M  *
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
       PROGRAM-ID.     STP097.
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
                       CONSOLE IS CRT.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY "DEPART.SL".

       COPY "PARAM.SL".

       COPY "STOCK.SL".

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

       COPY "DEPART.FDE".

       COPY "PARAM.FDE".

       COPY "STOCK.FDE".

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
       77  WS-SUB     PIC 9(04)   COMP-5.
       77  WS-S1            PIC 9(04)     COMP-5.
       77  WS-S2            PIC 9(04)     COMP-5.
       77  WS-S3            PIC 9(04)     COMP-5.
       77  WS-S4            PIC 9(04)     COMP-5.
       77  WS-S5            PIC 9(04)     COMP-5.
       77  WS-S6            PIC 9(04)     COMP-5.
       77  WS-S7            PIC 9(04)     COMP-5.
       77  WS-S8            PIC 9(04)     COMP-5.
       77  WS-PARKEY     PIC 9(04)   COMP-5.
       77  WS-OPTION        PIC X(01).
       77  WS-ERROR         PIC 9(01).
       77  WS-PASSWORD     PIC X(08).
       77  WS-PASS          PIC X(08).
       77  WS-DESC-H1     PIC  X(14) VALUE "Description".
       77  WS-DESC-H2     PIC  X(14) VALUE "Description 2".
       77  WS-RT-HD     PIC  X(10) VALUE "-Retail   ".
       77  WS-WS-HD     PIC  X(10) VALUE "-Wholesale".
       77  WS-CS-HD     PIC  X(10) VALUE "-Cash sale".
       77  WS-EKEY     PIC Z(04)9.
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "STP097".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "STOCK/INVENTORY -PRICE ADJUSTMENTS".
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

       COPY "W05.VAT".

       COPY "W05.RND".

       COPY "W10.STK".

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

       01  W15-MARKUPS.
           03  W15-LEDG                          OCCURS 300.
               05  W15-CODE        PIC  X(04).
               05  W15-MARKUP      PIC  9(03)V99 COMP-3.
               05  W15-CMRKUP      PIC  9(03)V99 COMP-3.
               05  W15-WMRKUP      PIC  9(03)V99 COMP-3.

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
       AA000         SECTION.
       AA00.
      *
      *   **** Check the User's security level for Stock/Inventory
      *
          IF LS0-STLEV < 9
      *
      *      Insufficient security level
      *
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
               GO TO AA10.
             PERFORM ZA000.
             PERFORM BA000.

       AA10.
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

      *
      *    ****    R E A D   F I L E S   R O U T I N E S
      *
       AC000-READ              SECTION.

       COPY "DEPART.RD".

       COPY "PARAM.RD".

       COPY "STOCK.RD".

      *
      *   R E W R I T E   &    W R I T E   F I L E S   R O U T I N E S
      *
       AD000-WRITE             SECTION.

       COPY "STOCK.WR".

    COPY "ROUND.PRC".

      /
       BA000           SECTION.
       BA00.
             PERFORM ERASE-SCREEN.
             DISPLAY "Stock Price and Markup Adjustments" AT 0224
                      WITH FOREGROUND-COLOR Grey HIGHLIGHT.
             DISPLAY "Have the Sales Ledger Markups been checked Y/N"
                      AT 0412 WITH FOREGROUND-COLOR Brown HIGHLIGHT.

       BA05.
             ACCEPT WS-OPTION AT 0459 WITH FOREGROUND-COLOR Grey HIGHLIGHT AUTO.
             CALL "CBL_TOUPPER" USING WS-OPTION BY VALUE WS-LENGTH RETURNING WS-STATUS.
           IF NOT(WS-OPTION = "N" OR "Y")
               GO TO BA05.
           IF WS-OPTION = "N"
               GO TO BA999.
             DISPLAY "Sales Ledger" AT 0612.
             MOVE 1                  TO WS-S1.
             MOVE "   !"             TO DPT-CODE.
             PERFORM START-AT-DEPT-CODE THRU READ-DEPART-EXIT.
           IF WS-F-ERROR = 7
               GO TO BA999.

       BA10.
             PERFORM READ-DEPART-NEXT THRU READ-DEPART-EXIT.
           IF WS-F-ERROR = 7
               GO TO BA15.
           IF (DPT-CODE = "XXXX" OR "YYYY") OR (DPT-SCDE = "VAT")
               GO TO BA10.
             DISPLAY DPT-CODE AT 0625 WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
             MOVE DPT-CODE           TO W15-CODE(WS-S1).
             MOVE DPT-MARKUP         TO W15-MARKUP(WS-S1).
           IF DPT-CMRKUP = ZERO
               MOVE DPT-MARKUP       TO DPT-CMRKUP.
             MOVE DPT-CMRKUP         TO W15-CMRKUP(WS-S1).
           IF DPT-WMRKUP = ZERO
               MOVE DPT-CMRKUP       TO DPT-WMRKUP.
             MOVE DPT-WMRKUP         TO W15-WMRKUP(WS-S1).
             ADD 1                   TO WS-S1.
             GO TO BA10.

       BA15.
             SUBTRACT 1 FROM WS-S1   GIVING WS-S7.
           IF WS-S7 = ZERO
               MOVE "No departments loaded" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO BA999.
             MOVE "   !"             TO STK-CODE.
             DISPLAY "Stock Record" AT 0812.
             PERFORM START-AT-STOCK-CODE THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 22
               GO TO BA999.

       BA20.
             PERFORM READ-STOCK-NEXT-LOCK THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 22
               GO TO BA999.
             DISPLAY STK-CODE AT 0825 WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
           IF STK-IND = 4
               UNLOCK STOCK
               GO TO BA20.
             MOVE 1                  TO WS-S1.

       BA25.
           IF STK-LDG = W15-CODE(WS-S1)
               GO TO BA30.
      *
      *    ****    I F   C O D E   N O T   O N   F I L E  -  R E A D   
      *                T H E   N E X T   S T O C K   R E C O R D
      *
           IF STK-LDG < W15-CODE(WS-S1)
               GO TO BA20.
           IF WS-S1 < WS-S7
               ADD 1                 TO WS-S1
               GO TO BA25
           ELSE
      *
      *    ****    I F   C O D E   N O T   O N   F I L E  -  R E A D   
      *                T H E   N E X T   S T O C K   R E C O R D
      *
               GO TO BA20.

       BA30.
      *    ****    A V E R A G E   C O S T
           IF STK-QUANT < 0.001
               MOVE STK-COST         TO STK-AVGE.
             MOVE STK-COST           TO W10-CCOST.
           IF W10-CCOST = ZERO
                MOVE STK-SELL        TO STK-CASH STK-WSALE
                GO TO BA35.
           IF W15-MARKUP (WS-S1) = ZERO
               MOVE STK-MARKUP       TO STK-CMARKUP STK-WMARKUP
               MOVE STK-SELL         TO STK-CASH STK-WSALE
               GO TO BA35.
             MOVE W15-MARKUP(WS-S1)  TO STK-MARKUP  W10-MARKUP.
             MOVE W15-CMRKUP(WS-S1)  TO STK-CMARKUP W10-CMARKUP.
             MOVE W15-WMRKUP(WS-S1)  TO STK-WMARKUP W10-WMARKUP.
             COMPUTE W10-CSELL ROUNDED = (W10-CCOST + ((W10-CCOST * W10-MARKUP) / 100)).
             MOVE W10-CSELL          TO W10-SELL.
             COMPUTE W10-CSELL ROUNDED = (W10-CCOST + ((W10-CCOST * W10-CMARKUP) / 100)).
             MOVE W10-CSELL          TO W10-CASH.
             COMPUTE W10-CSELL ROUNDED = (W10-CCOST + ((W10-CCOST * W10-WMARKUP) / 100)).
             MOVE W10-CSELL          TO W10-WSALE.
           IF W05-ROUND = "E"
      *
      *        R O U N D   T H E   E X C L U S I V E   P R I C E S
      *
               MOVE W10-SELL         TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL        TO W10-SELL
               MOVE W10-WSALE        TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL        TO W10-WSALE
               MOVE W10-CASH         TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL        TO W10-CASH.
           IF STK-TAX NOT = ZERO
               MOVE W05-VAT(STK-TAX) TO W05-RATE
               COMPUTE W10-VSELL  ROUNDED = W10-SELL  + (W10-SELL  * W05-RTE)
               COMPUTE W10-VWSALE ROUNDED = W10-WSALE + (W10-WSALE * W05-RTE)
               COMPUTE W10-VCASH  ROUNDED = W10-CASH  + (W10-CASH  * W05-RTE)
           ELSE
               MOVE W10-WSALE        TO W10-VWSALE
               MOVE W10-CASH         TO W10-VCASH
               MOVE W10-SELL         TO W10-VSELL.
           IF W05-ROUND = "I"
      *
      *        R O U N D   T H E   I N C L U S I V E   P R I C E S
      *
               MOVE W10-VSELL        TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL        TO W10-VSELL
               MOVE W10-VWSALE       TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL        TO W10-VWSALE
               MOVE W10-VCASH        TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL        TO W10-VCASH
               PERFORM CB200.
             MOVE W10-CASH           TO W10-VCASH
             MOVE W10-SELL           TO W10-VSELL
             MOVE W10-WSALE          TO W10-VWSALE.
             MOVE ZERO               TO W05-RATE.
           IF W10-TAX > 0
               MOVE W05-VAT(W10-TAX) TO W05-RATE.
             COMPUTE W10-VCASH  ROUNDED = W10-VCASH  + (W10-VCASH  * W05-RTE).
             COMPUTE W10-VWSALE ROUNDED = W10-VWSALE + (W10-VWSALE * W05-RTE).
             COMPUTE W10-VSELL  ROUNDED = W10-VSELL  + (W10-VSELL  * W05-RTE).
             MOVE W10-SELL           TO STK-SELL.
             MOVE W10-CASH           TO STK-CASH.
             MOVE W10-WSALE          TO STK-WSALE.

       BA35.
             PERFORM REWRITE-STOCK-UNLOCK THRU WRITE-STOCK-EXIT.
             GO TO BA20.

       BA999.
             EXIT.
      *    *************************************************************
      *                I N I T I A L I S E   P R O G R A M
      *    *************************************************************
       ZA000              SECTION.
       ZA00.
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
             MOVE 5                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-ROUND          TO W05-ROUND.
             MOVE PAR-ROUND-VAL      TO W05-ROUND-VAL.
           IF W05-ROUND-VAL > 0.01
               COMPUTE W05-HLF-VAL = W05-ROUND-VAL / 2
           ELSE
               MOVE ZERO             TO W05-HLF-VAL.
           IF W05-HLF-VAL > 0.01
               COMPUTE W05-TQTR-VAL = W05-HLF-VAL * 1.5
           ELSE
               MOVE ZERO             TO W05-TQTR-VAL.
               MOVE 1                TO W05-V-RATE.

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
               WHEN 7     MOVE W02-DEPART   TO WS-FILE
                          MOVE DPT-CODE     TO WS-KEYX
               WHEN 15    MOVE WS-PARID     TO WS-FILE
                          MOVE WS-PARKEY    TO WS-KEY
               WHEN 22    MOVE W02-STOCKF   TO WS-FILE
                          MOVE STK-CODE     TO WS-KEYX
               WHEN OTHER MOVE "ERROR"      TO WS-FILE
                          MOVE "STP097"     TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
