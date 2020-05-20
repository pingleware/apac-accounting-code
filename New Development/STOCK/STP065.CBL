      $set LINKCOUNT"512" GNT"STP065.GNT"
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
      *   **           **     **    **  **    **  **        *******    *
      *    ******      **     *******   **    **  ********        **   *
      *         **     **     **        **    **  **    **        **   *
      *   **    **     **     **         **  **   **    **  **    **   *
      *    ******      **     **          ****     ******    ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *  A D D   S T O C K   R E C O R D S   T O   W A R E H O U S E   *
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
       PROGRAM-ID.     STP065.
       AUTHOR.         J.W.LEMMON (JWL Software).
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

                You should have received a COPY of the GNU General
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

       COPY "STKMEM.SL".

       COPY "STOCK.SL".

       COPY "WARHSE.SL".

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

       COPY "STKMEM.FDE".

       COPY "STOCK.FDE".

       COPY "WARHSE.FDE".

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
       77  WS-RECORDS              PIC  9(06)    COMP-5.
       77  WS-ETYPE                PIC  X(01).
       77  WS-OPTION               PIC  X(01).
       77  WS-USE-ITM              PIC  X(01).
       77  WS-EXT-STK              PIC  X(01).
       77  WS-SKIP                 PIC  X(01).
       77  WS-ADJ                  PIC  X(01).
       77  WS-ERROR                PIC  9(01).
       77  WS-AMEND                PIC  9(01)           VALUE 0.
       77  WS-CONREC               PIC  9(01)           VALUE ZERO.
       77  WS-LEDG                 PIC  X(04).
       77  WS-COMP                 PIC  9(01).
       77  WS-IND                  PIC  9(01)           VALUE ZERO.
       77  WS-TYPE                 PIC  X(01).
       77  WS-PASSWORD             PIC  X(08).
       77  WS-PRINT                PIC  9(01).
       77  WS-PASS                 PIC  X(08).
       77  WS-DESC-H1              PIC  X(14)           VALUE "Description".
       77  WS-DESC-H2              PIC  X(14)           VALUE "Description 2".
       77  WS-START                PIC  X(18).
       77  WS-END                  PIC  X(18).
       77  PRG-NAME                PIC  X(12)           VALUE SPACES.
       77  PRG-STOCK-LUP           PIC  X(12)           VALUE "STP\STPLOOK".
       77  PRG-WARHS-LUP           PIC  X(12)           VALUE "STP\WARLOOK".
       77  TODAY-DDMMYY            PIC  9(08)    COMP-5.
       77  WS-USUB                 PIC  9(04)    COMP-5.

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
           03  CRT-PROGRAM         PIC  X(15) VALUE "STP065".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "STOCK/INVENTORY -ADD ITEMS TO WAREHOUSE".
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
           03  WS-PROG             PIC  X(03) VALUE "065".

       01  WS-PARID.
           03  WS-SYS-ID           PIC  X(03).

       COPY "W05.VAT".

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

       COPY "W15.STK".

       COPY "W40.WS".

       01  W40-WAREHOUSE.
           03  W40-NAME1.
               05  W40-NAME        PIC  X(40).
           03  W40-NAME2                         REDEFINES W40-NAME1.
               05  W40-NCHAR                     OCCURS 40 PIC X(01).
           03  W40-CONTACT         PIC  X(30).
           03  W40-CODE            PIC  X(06).
           03  W40-CODE1           PIC  X(06).
           03  W40-CODE2           PIC  X(06).
           03  W40-ADD.
               05  W40-ADDRESS1.
                   07  W40-ADD1    PIC  X(30).
                   07  W40-ADD2    PIC  X(30).
                   07  W40-ADD3    PIC  X(30).
                   07  W40-ADD4    PIC  X(30).
               05  W40-ADDRESS2                  REDEFINES W40-ADDRESS1.
                   07  W40-ACHAR   PIC  X(01)    OCCURS 120.
           03  W40-POST            PIC  9(04).
           03  W40-TEL             PIC  X(14).
           03  W40-FAX             PIC  X(14).

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       01  W60-ADD.
           03  W60-ADDRESS.
               05  W60-ACHAR OCCURS 70 PIC X(01).
           03  W60-ADDR REDEFINES W60-ADDRESS.
               05  W60-ADD     PIC X(60).
               05  FILLER      PIC X(10).

       01  W75-KEYS.
           03  W75-S               PIC  9(02)    COMP-5.
           03  W75-S1              PIC  9(02)    COMP-5.
           03  W75-S2              PIC  9(02)    COMP-5.
           03  W75-WHOUSE                               OCCURS 18.
               05  W75-WHCDE       PIC  X(06).
           03  W75-ITEMS                                OCCURS 18.
               05  W75-CODE        PIC  X(14).
               05  W75-ITM         PIC  X(03).
               05  W75-SELL        PIC  9(07)V99 COMP-3.
               05  W75-VSELL       PIC  9(07)V99 COMP-3.
               05  W75-CASH        PIC  9(07)V99 COMP-3.
               05  W75-VCASH       PIC  9(07)V99 COMP-3.
               05  W75-WSALE       PIC  9(07)V99 COMP-3.
               05  W75-VWSALE      PIC  9(07)V99 COMP-3.
           03  W75-SLEDG           PIC  9(03)    COMP-3 OCCURS 10.

       01  W80-EDIT.
          03  W80-Q1.
              05  W80-S7V4   PIC Z(06)9.9999-.

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

       01  S01.
           03  LINE  6 COLUMN  5 VALUE "Warehouse   ".
           03          COLUMN 18 VALUE              "      "         BACKGROUND-COLOR Magenta.
           03  LINE  8 COLUMN  5 VALUE "Stock Details"               FOREGROUND-COLOR Brown HIGHLIGHT.
           03  LINE  9 COLUMN  5 VALUE "Sales Ledger".
           03          COLUMN 18 VALUE              "   "            BACKGROUND-COLOR Magenta.
           03  LINE 10 COLUMN  5 VALUE "Start Item  ".
           03          COLUMN 18 VALUE              "              " BACKGROUND-COLOR Magenta.
           03  LINE 11 COLUMN  5 VALUE "End Item    ".
           03          COLUMN 18 VALUE              "              " BACKGROUND-COLOR Magenta.

       01  S01-DATA.
           03  BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT.
               05  S02.
                   07  LINE  6 COLUMN 18 PIC X(06) USING W40-CODE AUTO.

               05  S03.
                   07  LINE  9 COLUMN 18 PIC X(04) USING W10-LEDG AUTO.

               05  S05.
                   07  COLUMN 18 PIC X(14) USING W10-ITEM AUTO.

               05  S05-ITM.
                   07  COLUMN 18 PIC X(14) USING W10-ITEM AUTO.
                   07  COLUMN 33 PIC X(03) USING W10-ITM AUTO.

               05  S05-EXT.
                   07  COLUMN 18 PIC X(18) USING W10-EXT-ITEM AUTO.

               05  S05-3.
                   07  COLUMN 18 PIC Z(03) USING W10-I3 AUTO.

               05  S05-4.
                   07  COLUMN 18 PIC Z(04) USING W10-I4 AUTO.

               05  S05-5.
                   07  COLUMN 18 PIC Z(05) USING W10-I5 AUTO.

               05  S05-6.
                   07  COLUMN 18 PIC Z(06) USING W10-I6 AUTO.

               05  S05-7.
                   07  COLUMN 18 PIC Z(07) USING W10-I7 AUTO.

               05  S05-8.
                   07  COLUMN 18 PIC Z(08) USING W10-I8 AUTO.

               05  S05-9.
                   07  COLUMN 18 PIC Z(09) USING W10-I9 AUTO.

               05  S05-10.
                   07  COLUMN 18 PIC Z(10) USING W10-I10 AUTO.

               05  S05-11.
                   07  COLUMN 18 PIC Z(11) USING W10-I11 AUTO.

               05  S05-12.
                   07  COLUMN 18 PIC Z(12) USING W10-I12 AUTO.

               05  S05-13.
                   07  COLUMN 18 PIC Z(13) USING W10-I13 AUTO.

               05  S05-14.
                   07  COLUMN 18 PIC Z(14) USING W10-I14 AUTO.

       COPY "DEPART.CRT".

       01  S-WLUP.
           03   BACKGROUND-COLOR Cyan FOREGROUND-COLOR BLUE.
                05  LINE 50 COLUMN 02 VALUE  "F1".                                            FOREGROUND-COLOR Brown HIGHLIGHT.
                05          COLUMN 04 VALUE    "=Help,".
                05          COLUMN 02 VALUE          "F2"                                     FOREGROUND-COLOR Brown HIGHLIGHT.
                05          COLUMN 02 VALUE            " to do Warehouse Lookup,".
                05          COLUMN 02 VALUE                                    "Esc"          FOREGROUND-COLOR Brown HIGHLIGHT.
                05          COLUMN 02 VALUE                                       " to exit".

       COPY "WARLUP.CRT".

       01  S-LEDG.
           03  BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
               05  LINE 50 COLUMN 02 VALUE "F1"                             FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 04 VALUE   "=Help,".
               05          COLUMN 10 VALUE         "F6"                     FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 12 VALUE           "=Sales Ledger Lookup".
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
       AA49.
             EXIT PROGRAM.

       COPY "AA50SW.LUP".

       COPY "AA300.STK".

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
      *    ³   ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿      ³
      *    ³   ³           Print transactions (Y/N) [ ]         ³°°    ³
      *    ³   ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ°°    ³
      *    ³     °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°    ³
      *    ³                                                           ³
      *    ³ The response is returned in WS-OPTION (in upper case).    ³
      *    ³                                                           ³
      *    ³ To display the request message higher or lower (default   ³
      *    ³ is line 20) move the line number to be used to            ³
      *    ³ WS-MES-LINE.                                              ³
      *    ³                                                           ³
      *    ³ If character 48 of the message contains a value of X"FA"  ³
      *    ³ then do not display the message in a window, only get the ³
      *    ³ User reponse at the position specified by WS-MES-LINE and ³
      *    ³ WS-MES-COL.                                               ³
      *    ³                                                           ³
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

      *
      *    ****    R E A D   F I L E S   R O U T I N E S
      *
       AC000              SECTION.

       COPY "CONTROL.RD".

       COPY "DEPART.RD".

       COPY "PARAM.RD".

       COPY "STKMEM.RD".

       COPY "STOCK.RD".

       COPY "WARHSE.RD".

       COPY "WSTOCK.RD".

      *
      *   R E W R I T E   &    W R I T E   F I L E S   R O U T I N E S
      *
       AD000             SECTION.

       COPY "CONTROL.WR".

       COPY "PARAM.WR".

       COPY "STOCK.WR".

       COPY "WARHSE.WR".

       COPY "WSTOCK.WR".

       COPY "DEPART.LUP".

       COPY WARHSE.LUP.

       COPY "APAC.HLP".

       COPY "CA000.STK".

       COPY "CA100.STK".

       COPY "CA200.STK".

       COPY "CA500.STK".

      *
      *    ****   G E T   W A R E H O U S E   R E C O R D
      *
       CA600      SECTION.
       CA601.
            MOVE 6   TO W50-LENGTH.

       CA605.
            MOVE W40-CODE  TO W50-DATA.
           IF W50-1 = SPACE
        MOVE W50-5  TO W40-CODE
        GO TO CA605.
      CALL "CBL_TOUPPER" USING W50-DATA
                                BY VALUE W50-LENGTH
                                RETURNING W50-STATUS.
            MOVE W50-DENT  TO W40-CODE.

       CA606.
            MOVE W50-DENT  TO WAR-CODE.

       CA610.
      PERFORM READ-WARHSE THRU READ-WARHSE-EXIT.

       CA699.
             EXIT.
      /
      *    ****   C R E A T E   W / H   S T O C K   R E C O R D S
      *
       CB000   SECTION 50.
       CB00.
            PERFORM ERASE-SCREEN.
            DISPLAY S01.
            MOVE SPACES              TO W40-CODE.
            MOVE SPACES              TO W10-LEDG.

       CB05.
             DISPLAY S-WLUP.
             ACCEPT S02.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY  GO TO CB999
                 WHEN F1-KEY   PERFORM HELP-ROUTINE
                               GO TO CB05
                 WHEN F2-KEY   PERFORM AA50W THRU AA59
                 WHEN OTHER    PERFORM AA900-ALARM
                               GO TO CB05
               END-EVALUATE
               DISPLAY S02
               IF W40-CODE = SPACE
                   GO TO CB05.
             PERFORM CLEAR-L50.
           IF W40-CODE = SPACE
               GO TO CB999.

       CB10.
             PERFORM CA600.
           IF WS-F-ERROR = 51
               MOVE "Z"              TO WS-ETYPE
               PERFORM AA50W THRU AA59
               DISPLAY S02
               IF W40-CODE = SPACES
                   GO TO CB05
               ELSE
                   GO TO CB10.
            MOVE WAR-NAME            TO W40-NAME.
      DISPLAY W40-NAME AT 0425
       WITH HIGHLIGHT BACKGROUND-COLOR Magenta.

       CB15.
             DISPLAY S-LEDG.
             ACCEPT S03.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY  GO TO CB05
                 WHEN F1-KEY   PERFORM HELP-ROUTINE
                               GO TO CB15
                 WHEN F6-KEY   PERFORM SAVE-SCREEN
                               PERFORM DEPART-VIEW
                               PERFORM RESTORE-SCREEN
                               GO TO CB15
                 WHEN OTHER    PERFORM AA900-ALARM
                               GO TO CB15
               END-EVALUATE.
             MOVE W10-LEDG           TO DPT-CODE.
             PERFORM READ-DEPART THRU READ-DEPART-EXIT.
           IF DPT-DESC = SPACES
               MOVE "NO SUCH   D E P A R T M E N T" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO CB15.
             DISPLAY DPT-DESC AT 0922 WITH HIGHLIGHT BACKGROUND-COLOR Magenta.

       CB20.
             MOVE SPACE   TO W10-EXT-ITEM.
           IF W10-STCK = "N"
               MOVE ZERO             TO W10-I14.
      *        EVALUATE W10-SLNGTH
      *          WHEN 3   MOVE ZERO  TO W10-I3            
      *          WHEN 4   MOVE ZERO  TO W10-I4            
      *          WHEN 5   MOVE ZERO  TO W10-I5            
      *          WHEN 6   MOVE ZERO  TO W10-I6            
      *          WHEN 7   MOVE ZERO  TO W10-I7            
      *          WHEN 8   MOVE ZERO  TO W10-I8            
      *          WHEN 9   MOVE ZERO  TO W10-I9            
      *          WHEN 10  MOVE ZERO  TO W10-I10            
      *          WHEN 11  MOVE ZERO  TO W10-I11            
      *          WHEN 12  MOVE ZERO  TO W10-I12            
      *          WHEN 13  MOVE ZERO  TO W10-I13            
      *          WHEN 14  MOVE ZERO  TO W10-I14
      *        END-EVALUATE.
             MOVE W10-EXT-ITEM       TO WS-START WS-END.

       CB25.
             PERFORM AA300.
      *    *************************************************************
      *            G E T   S T A R T I N G   S T O C K   C O D E
      *    *************************************************************
      *    
      *      Is an 18 character stock/inventory item code being used?  
      *    
           IF WS-EXT-STK = "Y"
               ACCEPT S05-EXT AT LINE 10
           ELSE
      *   
      *       Is an ALPHA-NUMERIC code being used?
      *    
           IF W10-STCK = "A"
      *    
      *     Is a 3 character extension code being used in addition to 
      *     the 14 character stock/inventory item code?               
      *    
               IF WS-USE-ITM = "Y"
                   ACCEPT S05-ITM AT LINE 10
               ELSE
                   ACCEPT S05 AT LINE 10
               END-IF
      *    *************************************************************
      *                         NUMERIC CODES APPLY 
      *    *************************************************************
      *    
      *     Check the length of the numeric Stock/Inventory item code 
      *     being used and allow the User to key in the code.         
      *    
           ELSE
               EVALUATE W10-SLNGTH
      *                     3 Digit numeric Stock/Inventory item code
                 WHEN 3         ACCEPT S05-3 AT LINE 10
      *                     4 Digit numeric Stock/Inventory item code
                 WHEN 4         ACCEPT S05-4 AT LINE 10
      *                     5 Digit numeric Stock/Inventory item code
                 WHEN 5         ACCEPT S05-5 AT LINE 10
      *                     6 Digit numeric Stock/Inventory item code
                 WHEN 6         ACCEPT S05-6 AT LINE 10
      *                     7 Digit numeric Stock/Inventory item code
                 WHEN 7         ACCEPT S05-7 AT LINE 10
      *                     8 Digit numeric Stock/Inventory item code
                 WHEN 8         ACCEPT S05-8 AT LINE 10
      *                     9 Digit numeric Stock/Inventory item code
                 WHEN 9         ACCEPT S05-9 AT LINE 10
      *                    10 Digit numeric Stock/Inventory item code
                 WHEN 10        ACCEPT S05-10 AT LINE 10
      *                    11 Digit numeric Stock/Inventory item code
                 WHEN 11        ACCEPT S05-11 AT LINE 10
      *                    12 Digit numeric Stock/Inventory item code
                 WHEN 12        ACCEPT S05-12 AT LINE 10
      *                    13 Digit numeric Stock/Inventory item code
                 WHEN 13        ACCEPT S05-13 AT LINE 10
      *                    14 Digit numeric Stock/Inventory item code
                 WHEN 14        ACCEPT S05-14 AT LINE 10
               END-EVALUATE.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY  MOVE SPACES TO W10-EXT-ITEM
                               GO TO CB15
                 WHEN F1-KEY   PERFORM HELP-ROUTINE
                               GO TO CB25
                 WHEN F2-KEY   MOVE "I"    TO W10-ETYPE
                 WHEN F3-KEY   MOVE "A"    TO W10-ETYPE
                 WHEN F4-KEY   MOVE "D"    TO W10-ETYPE
                 WHEN F5-KEY   MOVE "2"    TO W10-ETYPE
                 WHEN F6-KEY   MOVE "X"    TO W10-ETYPE
                 WHEN OTHER    PERFORM AA900-ALARM
                               GO TO CB25
               END-EVALUATE
               PERFORM AA50-LOOKUP
      *    
      *                      E X T E N D E D   C O D E
      *    
      *      Is an 18 character stock/inventory item code being used?  
      *    
               IF WS-EXT-STK = "Y"
                   DISPLAY S05-EXT AT LINE 10
               ELSE
      *   
      *     Is an ALPHA-NUMERIC code being used?
      *    
               IF (W10-STCK = "A") AND (WS-USE-ITM = "Y")
      *    
      *     A 3 character extension code is being used in addition to 
      *     the 14 character stock/inventory item code.              
      *    
                   DISPLAY S05-ITM AT LINE 10
               ELSE
      *    
      *     A 1 TO 14 character Alpha-Numeric or Numeric code is being 
      *     used.               
      *    
                   DISPLAY S05 AT LINE 10
               END-IF
               IF W10-EXT-ITEM = SPACES
                   GO TO CB25.
            PERFORM CLEAR-L50.
          IF W10-EXT-ITEM = SPACE
              GO TO CB15.

       CB30.
      *
      *     Read Stock record using Item Code.
      *
             MOVE "I"                TO W10-INSTR.
             PERFORM CA000.


      *    
      *                      E X T E N D E D   C O D E
      *    
           IF WS-EXT-STK = "Y"
               DISPLAY S05-EXT AT LINE 10
           ELSE
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
           IF (W10-STCK = "A") AND (WS-USE-ITM = "Y")
               DISPLAY S05-ITM AT LINE 10
           ELSE
               DISPLAY S05 AT LINE 10.
           IF WS-F-ERROR = 22
               MOVE "Z"              TO W10-ETYPE
               PERFORM AA50-LOOKUP
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
               IF WS-EXT-STK = "Y"
                   DISPLAY S05-EXT AT LINE 10
               ELSE    
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
               IF (W10-STCK = "A") AND (WS-USE-ITM = "Y")
                   DISPLAY S05-ITM AT LINE 10
               ELSE
                   DISPLAY S05 AT LINE 10
               END-IF
               IF W10-EXT-ITEM = SPACES
                   GO TO CB25
               ELSE
                   GO TO CB30.
             DISPLAY STK-DESC AT 1037 WITH HIGHLIGHT BACKGROUND-COLOR Magenta.
             MOVE STK-CODE           TO WS-START.

       CB35.
             PERFORM AA300.
      *    *************************************************************
      *              G E T   E N D I N G   S T O C K   C O D E
      *    *************************************************************
      *    
      *      Is an 18 character stock/inventory item code being used?  
      *    
           IF WS-EXT-STK = "Y"
               ACCEPT S05-EXT AT LINE 11
           ELSE
      *   
      *       Is an ALPHA-NUMERIC code being used?
      *    
           IF W10-STCK = "A"
      *    
      *     Is a 3 character extension code being used in addition to 
      *     the 14 character stock/inventory item code?               
      *    
               IF WS-USE-ITM = "Y"
                   ACCEPT S05-ITM AT LINE 11
               ELSE
                   ACCEPT S05 AT LINE 11
               END-IF
      *    *************************************************************
      *                         NUMERIC CODES APPLY 
      *    *************************************************************
      *    
      *     Check the length of the numeric Stock/Inventory item code 
      *     being used and allow the User to key in the code.         
      *    
           ELSE
               EVALUATE W10-SLNGTH
      *                     3 Digit numeric Stock/Inventory item code
                 WHEN 3         ACCEPT S05-3 AT LINE 11
      *                     4 Digit numeric Stock/Inventory item code
                 WHEN 4         ACCEPT S05-4 AT LINE 11
      *                     5 Digit numeric Stock/Inventory item code
                 WHEN 5         ACCEPT S05-5 AT LINE 11
      *                     6 Digit numeric Stock/Inventory item code
                 WHEN 6         ACCEPT S05-6 AT LINE 11
      *                     7 Digit numeric Stock/Inventory item code
                 WHEN 7         ACCEPT S05-7 AT LINE 11
      *                     8 Digit numeric Stock/Inventory item code
                 WHEN 8         ACCEPT S05-8 AT LINE 11
      *                     9 Digit numeric Stock/Inventory item code
                 WHEN 9         ACCEPT S05-9 AT LINE 11
      *                    10 Digit numeric Stock/Inventory item code
                 WHEN 10        ACCEPT S05-10 AT LINE 11
      *                    11 Digit numeric Stock/Inventory item code
                 WHEN 11        ACCEPT S05-11 AT LINE 11
      *                    12 Digit numeric Stock/Inventory item code
                 WHEN 12        ACCEPT S05-12 AT LINE 11
      *                    13 Digit numeric Stock/Inventory item code
                 WHEN 13        ACCEPT S05-13 AT LINE 11
      *                    14 Digit numeric Stock/Inventory item code
                 WHEN 14        ACCEPT S05-14 AT LINE 11
               END-EVALUATE.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY  MOVE SPACES TO W10-EXT-ITEM
                               GO TO CB25
                 WHEN F1-KEY   PERFORM HELP-ROUTINE
                               GO TO CB35
                 WHEN F2-KEY   MOVE "I"    TO W10-ETYPE
                 WHEN F3-KEY   MOVE "A"    TO W10-ETYPE
                 WHEN F4-KEY   MOVE "D"    TO W10-ETYPE
                 WHEN F5-KEY   MOVE "2"    TO W10-ETYPE
                 WHEN F6-KEY   MOVE "X"    TO W10-ETYPE
                 WHEN OTHER    PERFORM AA900-ALARM
                               GO TO CB35
               END-EVALUATE
               PERFORM AA50-LOOKUP
      *    
      *                      E X T E N D E D   C O D E
      *    
      *      Is an 18 character stock/inventory item code being used?  
      *    
               IF WS-EXT-STK = "Y"
                   DISPLAY S05-EXT AT LINE 11
               ELSE
      *   
      *     Is an ALPHA-NUMERIC code being used?
      *    
               IF (W10-STCK = "A") AND (WS-USE-ITM = "Y")
      *    
      *     A 3 character extension code is being used in addition to 
      *     the 14 character stock/inventory item code.              
      *    
                   DISPLAY S05-ITM AT LINE 11
               ELSE
      *    
      *     A 1 TO 14 character Alpha-Numeric or Numeric code is being 
      *     used.               
      *    
                   DISPLAY S05 AT LINE 11
               END-IF
               IF W10-EXT-ITEM = SPACES
                   GO TO CB35.
            PERFORM CLEAR-L50.
          IF W10-EXT-ITEM = SPACE
              GO TO CB35.

       CB40.
      *
      *     Read Stock record using Item Code.
      *
             MOVE "I"                TO W10-INSTR.
             PERFORM CA000.
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
    IF W10-STCK = "A"
        IF WS-EXT-STK = "Y"
     DISPLAY S05-EXT AT LINE 9
        ELSE
        IF WS-USE-ITM = "Y"
     DISPLAY S05-ITM AT LINE 9
        ELSE
           DISPLAY S05 AT LINE 9
        END-IF
    ELSE
        DISPLAY S05 AT LINE 9.
          IF WS-F-ERROR = 22
        MOVE "Z"   TO W10-ETYPE
              PERFORM AA50-LOOKUP
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
        IF W10-STCK = "A"
     IF WS-EXT-STK = "Y"
         DISPLAY S05-EXT AT LINE 9
     ELSE
     IF WS-USE-ITM = "Y"
         DISPLAY S05-ITM AT LINE 9
     ELSE
         DISPLAY S05 AT LINE 9
     END-IF
        ELSE
     DISPLAY S05 AT LINE 9
        END-IF
              IF W10-EXT-ITEM = SPACES
           GO TO CB35
               ELSE
     GO TO CB40.
      DISPLAY STK-DESC AT 0937
       WITH HIGHLIGHT BACKGROUND-COLOR Magenta.
      MOVE STK-CODE  TO WS-END.
             PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
        GO TO CB05.
    IF WS-END < WS-START
        MOVE "End Item < Start Item"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO CB25.
      MOVE W10-LEDG  TO STK-LDG.
      MOVE WS-START  TO STK-CODE.
      PERFORM START-AT-STOCK-LEDG THRU READ-STOCK-EXIT.
    IF WS-F-ERROR = 22
        GO TO CB50.
      DISPLAY "Adding :" AT 1205.
       CB45.
      PERFORM READ-STOCK-NEXT THRU READ-STOCK-EXIT.
    IF WS-F-ERROR = 22
        GO TO CB55.
    IF (W10-LEDG NOT = STK-LDG) OR
       (STK-CODE > WS-END)
        GO TO CB55.
    IF STK-IND > ZERO
        GO TO CB45.
      DISPLAY STK-CODE AT 1214 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
      DISPLAY STK-DESC AT 1233 WITH FOREGROUND-COLOR Grey HIGHLIGHT.
      MOVE W40-CODE  TO WST-WAR.
      MOVE STK-CODE  TO WST-EXT-CODE.
      PERFORM READ-WSTOCK THRU READ-WSTOCK-EXIT.
    IF WS-F-ERROR = 0
        GO TO CB45.
      INITIALIZE WST-RECORD1.
      MOVE W40-CODE  TO WST-WAR.
      MOVE STK-CODE  TO WST-EXT-CODE.
      MOVE SPACES  TO WST-BIN.
      MOVE ZERO   TO WST-MTD WST-MTDV WST-QUANT
        WST-YTD WST-YTDV.
      PERFORM WRITE-WSTOCK THRU WRITE-WSTOCK-EXIT.
      GO TO CB45.

       CB50.
      MOVE "No details in range"
     TO WS-ERR-MES.
            PERFORM ERROR-MESSAGE.

       CB55.
            MOVE "'C'ontinue, 'E'xit  [ ]"
     TO WS-ERR-MES.
      MOVE SPACES  TO WS-OPTION.
      PERFORM OPT-MESSAGE.
          IF NOT (WS-OPTION = "C" OR "E")
        GO TO CB55.
    IF WS-OPTION = "C"
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
            MOVE 3   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
    IF NOT (PAR-STOCK = "A" OR "N")
        MOVE "A"   TO PAR-STOCK.
      MOVE PAR-STOCK  TO W10-STCK.
    IF PAR-STOCK = "N"
        MOVE PAR-SLNGTH  TO W10-SLNGTH
    ELSE
        MOVE 14   TO W10-SLNGTH.
    IF W10-SLNGTH < 3
        MOVE 3   TO W10-SLNGTH
    ELSE
    IF W10-SLNGTH > 14
        MOVE 14   TO W10-SLNGTH.
      MOVE PAR-USE-ITM  TO WS-USE-ITM.
      MOVE PAR-EXT-STK  TO WS-EXT-STK.
            MOVE 1   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-COMPANY    TO W95-COMP.
             MOVE PAR-DMY        TO W12-TODAY.
             MOVE PAR-YMD        TO W12-T-YMD.
      MOVE 1   TO W05-V-RATE.

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
               WHEN 22    MOVE W02-STOCKF   TO WS-FILE
                          MOVE STK-CODE     TO WS-KEYX
               WHEN 25    MOVE W02-STKMEM   TO WS-FILE
                          MOVE SME-KEY      TO WS-KEYX
          ELSE
          IF WS-F-ERROR = 51
              MOVE W02-WARHSE TO WS-FILE
               MOVE ZERO         TO WS-KEY
        MOVE WAR-CODE  TO WS-KEYX
          ELSE
          IF WS-F-ERROR = 52
              MOVE W02-WSTOCK TO WS-FILE
               MOVE ZERO         TO WS-KEY
        MOVE WST-KEY  TO WS-KEYX
               WHEN OTHER MOVE "ERROR"      TO WS-FILE
                          MOVE "STP065"     TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
