      $SET LINKCOUNT"384"
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
      *    ******   ********  *******   *******    ******   *******    *
      *   **    **     **     **    **  **    **  **    **  **    **   *
      *   **           **     **    **  **    **  **    **  **    **   *
      *    ******      **     *******   *******   **    **  ******     *
      *         **     **     **        **        **    **  **   **    *
      *   **    **     **     **        **        **    **  **    **   *
      *    ******      **     **        **         ******   **    **   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     P U R C H A C E S   T R A C K I N G   P R O G R A M        *
      *     C A L L E D   B Y   S T O C K   O R D E R S   O R          *
      *     E N Q U I R I E S                                          *
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
       PROGRAM-ID.     STPPOR.
       AUTHOR.         J.W.LEMMON.
       DATE-WRITTEN.   OCTOBER 1992.

                COPYRIGHT NOTICE: COPYRIGHT (C) 1992 - 2018
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

       COPY "PARAM.SL".

       COPY "PURDEX.SL".

      $SET KEYCOMPRESS "7"
          SELECT PURCHIST ASSIGN DISK
                          STATUS WS-STATUS
                          ACCESS DYNAMIC
                          ORGANIZATION INDEXED
                          RECORD PUR-KEY
                          ALTERNATE RECORD PUR-MKEY =
                                    PUR-CODE PUR-DTE DUPLICATES
                          ALTERNATE RECORD PUR-COST
                                    DUPLICATES.
      $SET NOKEYCOMPRESS


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

       COPY "PARAM.FDE".

       COPY "PURDEX.FDE".

      /
       FD  PURCHIST  IS EXTERNAL
       LABEL RECORD STANDARD
             VALUE OF FILE-ID W02-PURCHIST.
       01  PUR-REC1.
          03  PUR-KEY.
        05  PUR-SUPP    PIC  X(06).
        05  PUR-EXT-CODE.
   07  PUR-CODE  PIC  X(14).
   07  PUR-ITM   PIC  X(03).
   07  FILLER    PIC  X(01).
        05  PUR-DTE     PIC  9(08)    COMP.
        05  PUR-SEQ     PIC  9(04)    COMP.
        05  PUR-TRAN    PIC  X(01).
    03  PUR-REF        PIC  X(08).
          03  PUR-QNT        PIC S9(07)V99 COMP-3.
    03  PUR-COST        PIC S9(09)V99 COMP-3.
    03  PUR-NAME.
        05  PUR-NME     PIC  X(29).
        05  FILLER      PIC  X(11).
    03  PUR-WHSE        PIC  X(06).
    03  FILLER        PIC  X(04).
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
       77  WS-SUB    PIC 9(04)  COMP-5.
       77  WS-S            PIC  9(04)    COMP-5.
       77  WS-S1           PIC  9(04)    COMP-5.
       77  WS-S2           PIC  9(04)    COMP-5.
       77  WS-S3           PIC  9(04)    COMP-5.
       77  WS-S4           PIC  9(04)    COMP-5.
       77  WS-S5           PIC  9(04)    COMP-5.
       77  WS-S6           PIC  9(04)    COMP-5.
       77  WS-S7           PIC  9(04)    COMP-5.
       77  WS-S8           PIC  9(04)    COMP-5.
       77  WS-S9           PIC  9(04)    COMP-5.
       77  WS-IXS1         PIC  9(04)    COMP-5.
       77  WS-PARKEY    PIC 9(06)  COMP-5.
       77  WS-USE-ITM    PIC X(01).
       77  WS-EXT-STK    PIC X(01).
       77  WS-PUR-STAT    PIC X(01) VALUE "N".
    88 CLOSE-PUR    VALUE "N".
       77  WS-OPTION       PIC  X(01).
       77  WS-ETYPE    PIC X(01).
       77  WS-SEQ    PIC X(01) VALUE "D".
       77  WS-ERROR        PIC  9(01) VALUE ZERO.
       77  WS-DESC-H1     PIC  X(14) VALUE "Description".
       77  WS-DESC-H2     PIC  X(14) VALUE "Description 2".
       77  WS-RT-HD     PIC  X(10) VALUE "-Retail   ".
       77  WS-WS-HD     PIC  X(10) VALUE "-Wholesale".
       77  WS-CS-HD     PIC  X(10) VALUE "-Cash sale".
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "STPPOR".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "STOCK/INVENTORY -PURCHASE TRACKING".
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
           03  WS-PROG             PIC  X(03) VALUE "POR".

       01  WS-PARID.
           03  WS-SYS-ID           PIC  X(03).

           03  W02-PURCHIST.
               05  FILLER          PIC  X(03) VALUE "PUR".
               05  W02-MTH56T      PIC  X(01) VALUE "X".
               05  FILLER          PIC  X(01) VALUE "_".
               05  W02-USE56T      PIC  X(03).
               05  FILLER          PIC  X(04) VALUE ".APC".

       01  W10-STOCK.
           03  W10-EXT-ITEM.
               05  W10-ITEM        PIC  X(14).
               05  W10-ITM         PIC  X(03).
               05  FILLER          PIC  X(01).
           03  W10-DATE.
               05  W10-EDTE        PIC  99/99/9999.
           03  W10-COST            PIC S9(07)V99 COMP-3.

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

       COPY "W15.WS".

       COPY "W25.STK".

       COPY "W40.WS".

       01  W45-TRAN.
           03  W45-CODE        PIC 9(02).
           03  W45-ENG         PIC X(12).
           03  W45-AFR         PIC X(12).
           03  W45-ACT         PIC X(01).

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       01  W75-KEYS.
           03  W75-S          PIC 9(02) COMP-5.
           03  W75-S1         PIC 9(02) COMP-5.
    03  W75-S2       PIC 9(02) COMP-5.
          03  W75-CNO       PIC X(06) OCCURS 18.
    03  W75-COST       PIC S9(07)V99 COMP-3 OCCURS 18.

       01  W95-STM.
           03  W95-D.
              05  W95-DTE     PIC Z9/99/9999.

       01  W100-EDIT.
          03  W100-COST       PIC Z(06)9.99.
    03  W100-QNT        PIC Z(04)9.99.

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

       01  W16-CALL.
    03  W16-EXT-ITEM.
        05  W16-ITEM  PIC  X(14).
        05  W16-ITM   PIC  X(03).
        05  FILLER    PIC  X(01).
    03  W16-CRED      PIC  X(06).
    03  W16-COST      PIC S9(07)V99 COMP-3.
    03  W16-CANCEL    PIC  X(01).
    03  W16-JCANCEL   PIC  X(01).

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

       01  S05D.
          03  LINE  2 COLUMN 32 FOREGROUND-COLOR Grey HIGHLIGHT VALUE
               "PURCHASE TRACKING".
    03  LINE  4 COLUMN 24 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown VALUE
    "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
    03  LINE  5 COLUMN 24 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown VALUE
    "³    Transactions for Period.    ³".
    03  LINE  6 COLUMN 24 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown VALUE
    "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".
    03  LINE  7 COLUMN 24 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown VALUE
    "³   Starting Date :              ³".
    03  LINE  8 COLUMN 24 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown VALUE
    "³   Ending Date   :              ³".
    03  LINE  9 COLUMN 24 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown VALUE
    "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".
    03  LINE 10 COLUMN 24 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown VALUE
    "³Dates to be keyed in as ".
    03        COLUMN 49 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown VALUE "DDMMCCYY".
    03        COLUMN 57 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown VALUE "³".
    03  LINE 11 COLUMN 24 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown VALUE
    "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S05A.
    03  LINE  7 COLUMN 44 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC 99/99/9999 USING W15-START AUTO.
    03  LINE  8 COLUMN 44 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC 99/99/9999 USING W15-END AUTO.

       01  S10D.
          03  LINE  2 COLUMN 20 FOREGROUND-COLOR Grey HIGHLIGHT VALUE
               "PURCHASE TRACKING MOVEMENT/REFERENCE LOOKUP".
    03  LINE  3 COLUMN  3 VALUE "Item Code.".

       01  S36.
          03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
      PIC X(14) USING W10-EXT-ITEM AUTO.

       01  S36-ITM.
          03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
           PIC X(14)/X(03)
     USING W10-EXT-ITEM AUTO.

       01  S36-EXT.
          03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
           PIC X(18) USING W10-EXT-ITEM AUTO.

       01  S-MOV.
           03  LINE  4 COLUMN  2 REVERSE-VIDEO VALUE
              "Ref. No.    Date    Ac/No  Cost Price  Quantity
      -        " Supplier Name        ".
          03  LINE  5 COLUMN  1 BACKGROUND-COLOR Black VALUE "Æ".
          03        COLUMN  2 VALUE "ÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÑÍÍÍÍÍ
      -        "ÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ".
    03        COLUMN 80 BACKGROUND-COLOR Black VALUE "µ".
          03  LINE 22 COLUMN  1 BACKGROUND-COLOR Black VALUE "Æ".
          03        COLUMN  2 VALUE "ÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ".
    03        COLUMN 80 BACKGROUND-COLOR Black VALUE "µ".

      *
      *      ******   ******    *****    *****   ******  ******   **   **  ******    ****** 
      *      **   **  **   **  **   **  **   **  **      **   **  **   **  **   **   **
      *      **   **  **  **   **   **  **       **      **   **  **   **  **  **    **
      *      ******   *****    **   **  **       *****   **   **  **   **  *****     *****
      *      **       **  **   **   **  **       **      **   **  **   **  **  **    **
      *      **       **   **  **   **  **   **  **      **   **  **   **  **   **   **
      *      **       **   **   *****    *****   ******  ******    *****   **   **   ******
      *
       PROCEDURE DIVISION
   USING LS-PARID LS-USER-ID LS0-PROGRAMS
         LS0-SECURITY W16-CALL.
       AA000-MAIN        SECTION.
       AA000-INIT.
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
      MOVE "Y"   TO W16-CANCEL.
      MOVE W16-EXT-ITEM  TO W10-EXT-ITEM.
      MOVE W16-COST  TO W10-COST.
            PERFORM ZA000-INIT.
      PERFORM BA000.
    IF CLOSE-PUR
        CLOSE PURDEX.
      CLOSE PURCHIST.

       AA49.
             EXIT PROGRAM.

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

       COPY "LOCKED.RC2".

       COPY "DATE.CHK".

       COPY "DATE.CNV".

      *
      *    ****    R E A D   F I L E S   R O U T I N E S
      *
       AC000    SECTION.

       COPY "PARAM.RD".

      *
      *    ****  *   * ****  ****  ***** *   *    ***** *** *    *****
      *    *   * *   * *   * *  * *  *   *    *   *  *    *
      *    *   * *   * *   * *  * *   * *    *   *  *    *
      *    ****  *   * ****  *  * ***    *    ***   *  *    ***
      *    *     *   * *   * *   * *      * *      *      *  *     *
      *    *  *   * *   * *  * *  *   *    *   *  *    *
      *    *   ***  *   * ****  ***** *   *    *  *** ***** *****
      *
       READ-PURDEX-NEXT.
            READ PURDEX NEXT.
           IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
              GO TO READ-PURDEX-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
        MOVE 56   TO WS-F-ERROR
        GO TO READ-PURDEX-EXIT.
           IF WS-STAT1 = "2" OR "3" OR "4"
              MOVE 56   TO WS-F-ERROR
               PERFORM READ-ERROR.
           IF RECORD-LOCKED
               PERFORM LOCKED-RECORD
              GO TO READ-PURDEX-NEXT.
            GO TO READ-PURDEX-EXIT.

       START-AT-PRDX-MOVE.
            START PURDEX
        KEY >= PRD-MKEY.
           IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
              GO TO READ-PURDEX-EXIT.
           IF WS-STATUS = "23"
        MOVE 56   TO WS-F-ERROR
        GO TO READ-PURDEX-EXIT.
           IF WS-STAT1 = "2" OR "3" OR "4"
              MOVE 56   TO WS-F-ERROR
               PERFORM READ-ERROR.

       READ-PURDEX-EXIT.
             EXIT.

       READ-PURCHIST-NEXT.
            READ PURCHIST NEXT.
           IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
              GO TO READ-PURCHIST-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
        MOVE 57   TO WS-F-ERROR
        GO TO READ-PURCHIST-EXIT.
           IF WS-STAT1 = "2" OR "3" OR "4"
              MOVE 57   TO WS-F-ERROR
               PERFORM READ-ERROR.
           IF RECORD-LOCKED
               PERFORM LOCKED-RECORD
              GO TO READ-PURCHIST-NEXT.
            GO TO READ-PURCHIST-EXIT.

       READ-PURCHIST-PREV.
            READ PURCHIST PREVIOUS.
           IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
              GO TO READ-PURCHIST-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
        MOVE 57   TO WS-F-ERROR
        GO TO READ-PURCHIST-EXIT.
           IF WS-STAT1 = "2" OR "3" OR "4"
              MOVE 57   TO WS-F-ERROR
               PERFORM READ-ERROR.
           IF RECORD-LOCKED
               PERFORM LOCKED-RECORD
              GO TO READ-PURCHIST-PREV.
            GO TO READ-PURCHIST-EXIT.

       START-AT-PURCHIST-MOVE.
            START PURCHIST
        KEY >= PUR-MKEY.
           IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
              GO TO READ-PURCHIST-EXIT.
           IF WS-STATUS = "23"
        MOVE 57   TO WS-F-ERROR
        GO TO READ-PURCHIST-EXIT.
           IF WS-STAT1 = "2" OR "3" OR "4"
              MOVE 57   TO WS-F-ERROR
               PERFORM READ-ERROR.
            GO TO READ-PURCHIST-EXIT.

       START-AT-PURCHIST-COST.
            START PURCHIST
        KEY >= PUR-COST.
           IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
              GO TO READ-PURCHIST-EXIT.
           IF WS-STATUS = "23"
        MOVE 57   TO WS-F-ERROR
        GO TO READ-PURCHIST-EXIT.
           IF WS-STAT1 = "2" OR "3" OR "4"
              MOVE 57   TO WS-F-ERROR
               PERFORM READ-ERROR.

       READ-PURCHIST-EXIT.
             EXIT.

      /
      *    ****    M O V E M E N T   L O O K U P
      *
       MOVEMENT-LOOKUP   SECTION.

       MOVE-LUP-SCREEN.
            DISPLAY S-MOV.
      INITIALIZE PUR-REC1.
      MOVE W10-EXT-ITEM TO PUR-EXT-CODE.
    IF WS-SEQ = "C"
        MOVE ZERO  TO PUR-COST
    ELSE
        MOVE W15-S-YMD  TO PUR-DTE.

       MOVE-LUP-CLR-SCREEN.
             MOVE 0601           TO CPOS.
            PERFORM MOVE-CRT-EDGE UNTIL CLIN = 22.

       MOVE-LUP-CONTINUE.
            MOVE 6   TO CLIN.
             MOVE 1              TO W75-S W75-S1.

       MOVE-LUP-TAB-LOOP.
      MOVE SPACES  TO W75-CNO (W75-S1).
      MOVE ZERO   TO W75-COST (W75-S1).
    IF W75-S1 < 18
               ADD 1             TO W75-S1
        GO TO MOVE-LUP-TAB-LOOP.
             MOVE ZERO           TO W75-S1.

       MOVE-LUP-START-PRDX.
      MOVE SPACES  TO WS-OPTION.
    IF WS-SEQ = "C"
              PERFORM START-AT-PURCHIST-COST THRU READ-PURCHIST-EXIT
    ELSE
              PERFORM START-AT-PURCHIST-MOVE THRU READ-PURCHIST-EXIT.
          IF WS-F-ERROR = 57
        GO TO MOVE-NO-RECORD.

       MOVE-GET-PURDEX-REC.
           IF WS-OPTION = "J"
               MOVE SPACE        TO WS-OPTION
        GO TO MOVE-SKIP-PRDX-READ.
            PERFORM READ-PURCHIST-NEXT THRU READ-PURCHIST-EXIT.
    IF WS-F-ERROR = 57
               SUBTRACT 1        FROM W75-S
        GO TO MOVE-FULL-SCREEN.

       MOVE-LUP-DISP-ITEM.
    IF NOT (PUR-EXT-CODE = W10-EXT-ITEM)
        GO TO MOVE-FULL-SCREEN.
    IF (PUR-DTE > W15-E-YMD)
        GO TO MOVE-FULL-SCREEN.

            MOVE-SKIP-PRDX-READ.
            MOVE PUR-SUPP  TO W75-CNO (W75-S).
            MOVE PUR-COST  TO W75-COST (W75-S).
            DISPLAY (CLIN, 2) PUR-REF WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
            MOVE PUR-DTE  TO W22-DTE2.
            PERFORM SWITCH-DATE-TO-DMCY THRU SWITCH-DATE-EXIT.
            MOVE W22-DTE3  TO W95-DTE.
            DISPLAY ( , 11) W95-DTE WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
            DISPLAY ( , 22) PUR-SUPP WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
      COMPUTE W100-COST ROUNDED = PUR-COST / PUR-QNT.
      *      MOVE PUR-COST  TO W100-COST.
      MOVE PUR-QNT  TO W100-QNT.
            DISPLAY ( , 29) W100-COST WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
      DISPLAY ( , 41) W100-QNT WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
      DISPLAY ( , 50) PUR-NME WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
      DISPLAY ( , 79) PUR-TRAN WITH FOREGROUND-COLOR Brown HIGHLIGHT.
           IF CLIN < 21
               ADD 1             TO CLIN W75-S
              GO TO MOVE-GET-PURDEX-REC.
      
       MOVE-FULL-SCREEN.
      DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR Cyan.
            DISPLAY " " AT 2303 WITH FOREGROUND-COLOR Brown HIGHLIGHT
             " - " WITH FOREGROUND-COLOR Grey
       "PgUp" WITH FOREGROUND-COLOR Brown HIGHLIGHT
                     "/" WITH FOREGROUND-COLOR Grey
       "PgDn" WITH FOREGROUND-COLOR Brown HIGHLIGHT
       " - " WITH FOREGROUND-COLOR Grey
             "ENTER" WITH FOREGROUND-COLOR Brown HIGHLIGHT
             " select - " WITH FOREGROUND-COLOR Grey
             "Esc" WITH FOREGROUND-COLOR Brown HIGHLIGHT
       "ape." WITH FOREGROUND-COLOR Grey.

       MOVE-SAVE-FULL-SCREEN.
      PERFORM SAVE-SCREEN-2.
            MOVE 0   TO W75-S1.
             MOVE 0601           TO CPOS.

       MOVE-CRT-ATTRIB.
      MOVE 3920   TO W41-LENGTH.
      CALL "CBL_WRITE_SCR_ATTRS" USING SAVE-POS
           W42-ATTRIB2
           W41-LENGTH
     RETURNING WS-STATUS.
            MOVE 4000   TO W41-LENGTH.

       MOVE-GET-USER-OPT.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
      PERFORM CLEAR-L50.
    IF ADIS-FUNC
        EVALUATE KEY-CODE-1
   WHEN UP-KEY
             GO TO MOVE-ADJ-NO-KEYS
   WHEN DOWN-KEY
             GO TO MOVE-CHK-NO-KEYS
   WHEN ENTER-KEY
   WHEN CR-KEY
       GO TO MOVE-CHK-SELECT
   WHEN OTHER  PERFORM AA900-ALARM
        END-EVALUATE
        GO TO MOVE-GET-USER-OPT
    ELSE
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO MOVEMENT-LOOKUP-EXIT
   WHEN PAGE-DOWN
             GO TO MOVE-GET-NEXT-PAGE
   WHEN PAGE-UP
             GO TO MOVE-GET-PREV-PAGE
   WHEN OTHER  PERFORM AA900-ALARM
        END-EVALUATE
        GO TO MOVE-GET-USER-OPT.
      PERFORM AA900-ALARM.
            GO TO MOVE-GET-USER-OPT.

       MOVE-GET-NEXT-PAGE.
           IF W75-S < 16
              GO TO MOVE-GET-USER-OPT.

       MOVE-NEW-PAGE.
             MOVE 0601           TO CPOS.
            PERFORM MOVE-CRT-EDGE UNTIL CLIN = 22.
             MOVE 6              TO CLIN.
             MOVE LOW-VALUES     TO W75-KEYS.
             MOVE 1              TO W75-S W75-S1.
      PERFORM MOVE-LUP-TAB-LOOP.
            GO TO MOVE-GET-PURDEX-REC.

       MOVE-GET-PREV-PAGE.
             MOVE 1              TO WS-IXS1.
      *
      *    ****    SET OPTION TO BYPASS THE READ NEXT
      *
       MOVE "J"    TO WS-OPTION.

       MOVE-GET-PREV-LOOP.
      PERFORM READ-PURCHIST-PREV THRU READ-PURCHIST-EXIT.
    IF WS-F-ERROR = 57
        MOVE SPACE  TO WS-OPTION
        GO TO MOVE-NEW-PAGE.
    IF NOT (PUR-EXT-CODE = W10-EXT-ITEM)
        PERFORM READ-PURCHIST-NEXT THRU READ-PURCHIST-EXIT
        GO TO MOVE-NEW-PAGE.
    IF PUR-DTE < W15-START
        PERFORM READ-PURCHIST-NEXT THRU READ-PURCHIST-EXIT
        GO TO MOVE-NEW-PAGE.
    IF WS-IXS1 < 32
        ADD 1   TO WS-IXS1
        GO TO MOVE-GET-PREV-LOOP.
      GO TO MOVE-NEW-PAGE.

       MOVE-NO-RECORD.
      PERFORM CLEAR-L50.
            DISPLAY "No record - Press " AT 5012
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
             "ANY" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
             " key"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
            GO TO MOVEMENT-LOOKUP-EXIT.

       MOVE-ADJ-NO-KEYS.
           IF W75-S1 > 1
               SUBTRACT 1        FROM W75-S1 CLIN.
            GO TO MOVE-HIGHLIGHT.

       MOVE-CHK-NO-KEYS.
           IF W75-S1 = ZERO
               MOVE 1            TO W75-S1
              GO TO MOVE-HIGHLIGHT.
           IF W75-S1 < W75-S
               ADD 1             TO W75-S1 CLIN.

       MOVE-HIGHLIGHT.
            PERFORM MOVE-CRT-ATTRIB.
             DISPLAY W43-DET (CLIN) AT CPOS WITH REVERSE-VIDEO.
            GO TO MOVE-GET-USER-OPT.

       MOVE-CHK-SELECT.
           IF W75-S1 = ZERO
        GO TO MOVE-GET-USER-OPT.
      MOVE CSTART  TO WS1-CURSOR.
      MOVE W75-CNO (W75-S1)
     TO W16-CRED.
      MOVE W75-COST (W75-S1)
     TO W16-COST.
      GO TO MOVEMENT-LOOKUP-EXIT.

       MOVE-CRT-EDGE.
            DISPLAY "³" AT CPOS WITH BACKGROUND-COLOR Black
          FOREGROUND-COLOR Cyan
       "        ³          ³      ³          ³         ³                            "
        WITH FOREGROUND-COLOR Cyan
       "³" WITH BACKGROUND-COLOR Black
         FOREGROUND-COLOR Cyan.
            ADD 1   TO CLIN.

       MOVEMENT-LOOKUP-EXIT.
             EXIT.

       COPY "APAC.HLP".

      /
      *  ****  E N Q U I R Y   ( P U R C H A S E   T R A C K I N G )
      *
       BA000         SECTION 50.
       BA00.
             PERFORM ERASE-SCREEN.
      MOVE 5   TO SHADE-ROW.
      MOVE 26   TO SHADE-COL.
      MOVE 32   TO SHADE-WIDTH.
      MOVE 7   TO SHADE-LINES.
      DISPLAY S05D.
      PERFORM SCREEN-SHADOW.
      MOVE W12-TODAY  TO W15-START W15-END.
    IF W15-SMNTH < 3
        ADD 12   TO W15-SMNTH
        SUBTRACT 1  FROM W15-SYEAR.
      SUBTRACT 2   FROM W15-SMNTH.
    IF W15-SMNTH = 02
        IF W15-SDAY > 28
     MOVE 28  TO W15-SDAY.
    IF W15-SMNTH = 04 OR 06 OR 09 OR 11
        IF W15-SDAY > 30
     MOVE 30  TO W15-SDAY.

       BA05.
      DISPLAY "Press " AT 5052
        WITH FOREGROUND-COLOR Blue BACKGROUND-COLOR Cyan
       "Esc" WITH FOREGROUND-COLOR Brown HIGHLIGHT
    BACKGROUND-COLOR Cyan
       "ape to Exit"
        WITH FOREGROUND-COLOR Blue BACKGROUND-COLOR Cyan.
      ACCEPT S05A.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO BA999
   WHEN OTHER  PERFORM AA900-ALARM
       GO TO BA05
        END-EVALUATE.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
        GO TO BA05.
      PERFORM CLEAR-L50.
             MOVE W15-START          TO W22-DATE.
             PERFORM DATE-CHECK THRU CHECK-DATE-EXIT.
           IF NOT(W22-ERROR = 0)
               EVALUATE W22-ERROR
                 WHEN 1  MOVE "Invalid Start DATE"  TO WS-ERR-MES
                 WHEN 2  MOVE "Invalid Start DAY"   TO WS-ERR-MES
                 WHEN 3  MOVE "Invalid Start MONTH" TO WS-ERR-MES
               END-EVALUATE
               PERFORM ERROR-MESSAGE
               GO TO BA05.
             MOVE W20-DTE            TO W15-S-YMD.
             MOVE W15-END            TO W22-DATE.
             PERFORM DATE-CHECK THRU CHECK-DATE-EXIT.
           IF NOT(W22-ERROR = 0)
               EVALUATE W22-ERROR
                 WHEN 1  MOVE "Invalid End DATE"  TO WS-ERR-MES
                 WHEN 2  MOVE "Invalid End DAY"   TO WS-ERR-MES
                 WHEN 3  MOVE "Invalid End MONTH" TO WS-ERR-MES
               END-EVALUATE
               PERFORM ERROR-MESSAGE
               GO TO BA05.
             MOVE W20-DTE        TO W15-E-YMD.
    IF W15-E-YMD < W15-S-YMD
        MOVE "End date is before start date"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO BA05.

       BA10.
             PERFORM ERASE-SCREEN.
      DISPLAY WS-BLNK78 AT 0302
       WITH FOREGROUND-COLOR Cyan BACKGROUND-COLOR Blue.
      DISPLAY S10D.

       BA15.
      *
      *    ****    D I S P L A Y   S T O C K   C O D E
      *
    IF WS-EXT-STK = "Y"
        DISPLAY S36-EXT AT 0314
    ELSE
    IF WS-USE-ITM = "Y"
              DISPLAY S36-ITM AT 0314
    ELSE
              DISPLAY S36 AT 0314.
      INITIALIZE PRD-REC1.
      MOVE W10-EXT-ITEM  TO PRD-EXT-CODE.
      MOVE W15-S-YMD  TO PRD-DTE.
      PERFORM SAVE-SCREEN.
      MOVE 13   TO SHADE-ROW.
      MOVE 28   TO SHADE-COL.
      MOVE 28   TO SHADE-WIDTH.
      MOVE 3   TO SHADE-LINES.
      DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" AT 1226
        WITH BACKGROUND-COLOR Grey
      FOREGROUND-COLOR Grey HIGHLIGHT.
      DISPLAY "³Extracting details requested³" AT 1326
        WITH BACKGROUND-COLOR Grey
      FOREGROUND-COLOR Grey HIGHLIGHT.
      DISPLAY "³Reading:                    ³" AT 1426
        WITH BACKGROUND-COLOR Grey
      FOREGROUND-COLOR Grey HIGHLIGHT.
      DISPLAY "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" AT 1526
        WITH BACKGROUND-COLOR Grey
      FOREGROUND-COLOR Grey HIGHLIGHT.
      PERFORM SCREEN-SHADOW.
            PERFORM START-AT-PRDX-MOVE THRU READ-PURDEX-EXIT.
          IF WS-F-ERROR = 56
        GO TO BA25.

       BA20.
            PERFORM READ-PURDEX-NEXT THRU READ-PURDEX-EXIT.
    IF WS-F-ERROR = 56
        GO TO BA25.
    IF NOT (PRD-EXT-CODE = W10-EXT-ITEM)
        GO TO BA25.
    IF (PRD-DTE > W15-E-YMD)
        GO TO BA25.
      DISPLAY PRD-EXT-CODE AT 1436
       WITH BACKGROUND-COLOR Magenta
     FOREGROUND-COLOR Cyan HIGHLIGHT.
      MOVE PRD-REC1  TO PUR-REC1.
      WRITE PUR-REC1.
      GO TO BA20.

       BA25.
      PERFORM RESTORE-SCREEN.

       BA30.
      MOVE "'C'ost price or 'D'ate Sequence  [C]"
     TO WS-ERR-MES.
      MOVE "C"   TO WS-OPTION.
      PERFORM OPT-MESSAGE.
          IF NOT (WS-OPTION = "C" OR "D")
        GO TO BA30.
      MOVE WS-OPTION  TO WS-SEQ.
      PERFORM MOVEMENT-LOOKUP.

       BA999.
             EXIT.
      *    *************************************************************
      *                I N I T I A L I S E   P R O G R A M
      *    *************************************************************
       ZA000-INIT        SECTION 9.
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
      MOVE AFID-PATH  TO W02-PARAM.
      MOVE "PURDEX"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-PURDEX.
      MOVE LS-USER  TO W02-USE56T.
            MOVE 3   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      MOVE PAR-USE-ITM  TO WS-USE-ITM.
      MOVE PAR-EXT-STK  TO WS-EXT-STK.
            OPEN I-O PURDEX.
    IF WS-STATUS = "41"
        MOVE "Y"   TO WS-PUR-STAT
        GO TO ZA05.
    IF WS-STATUS = "05"
        CLOSE PURDEX
        OPEN OUTPUT PURDEX
        CLOSE PURDEX
        OPEN I-O PURDEX.
          IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
           GO TO ZA49
               ELSE
        IF IDX-CORRUPT OR
   (WS-STATUS = "39")
     MOVE 56  TO WS-F-ERROR
     GO TO ZA50.
           IF WS-STATUS NOT = "00"
              MOVE 56   TO WS-F-ERROR
               PERFORM OPEN-ERROR.

       ZA05.
      OPEN OUTPUT PURCHIST.
          IF RUNTIME-ERROR
              IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
           GO TO ZA49
        ELSE
           MOVE 57  TO WS-F-ERROR
              PERFORM OPEN-ERROR.
      CLOSE PURCHIST.
      OPEN I-O PURCHIST.
            GO TO ZA999-EXIT.

       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
            GO TO ZA205.
       ZA50.
          IF WS-F-ERROR = 56
              MOVE "CARDEX file"
     TO WS-FILE.
             DISPLAY "Rebuild " AT 0812
                      WS-FILE WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             DISPLAY "Press any key to continue" AT 1012.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
       ZA51.
             EXIT PROGRAM.

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
               WHEN 15    MOVE WS-PARID     TO WS-FILE
                          MOVE WS-PARKEY    TO WS-KEY
           ELSE
          IF WS-F-ERROR = 56
              MOVE W02-PURDEX  TO WS-FILE
               MOVE ZERO         TO WS-KEY
        MOVE PRD-KEY  TO WS-KEYX
    ELSE
          IF WS-F-ERROR = 57
              MOVE W02-PURCHIST TO WS-FILE
               MOVE ZERO         TO WS-KEY
        MOVE PUR-KEY  TO WS-KEYX
               WHEN OTHER MOVE "ERROR"      TO WS-FILE
                          MOVE "STPPOR"     TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
