      $SET LINKCOUNT"384" GNT"STPJOB.GNT"
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
      *    ******   ********  *******   ********   ******   *******    *
      *   **    **     **     **    **     **     **    **  **    **   *
      *   **           **     **    **     **     **    **  **    **   *
      *    ******      **     *******      **     **    **  ******     *
      *         **     **     **           **     **    **  **    **   *
      *   **    **     **     **        ** **     **    **  **    **   *
      *    ******      **     **         ***       ******   *******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     J O B   D E T A I L S   P R O G R A M   C A L L E D   B Y  *
      *     S T O C K   E N Q U I R I E S                              *
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
       PROGRAM-ID.     STPJOB.
       AUTHOR.         J.W.LEMMON.
       DATE-WRITTEN.   SEPTEMBER 2005.

                COPYRIGHT NOTICE: COPYRIGHT (C) 2005 - 2018
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

       COPY "PARAM.SL".

       COPY "JOBCST.SL".

          SELECT JOBDET  ASSIGN DISK
                          LOCK MANUAL
                          WITH LOCK ON MULTIPLE RECORDS
                          STATUS WS-STATUS
                          ACCESS RANDOM
                          ORGANIZATION RELATIVE
           RELATIVE WS-JEXKEY.

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

       COPY "PARAM.FDE".

       COPY "JOBCST.FDE".

      /
       FD  JOBDET    IS EXTERNAL
       LABEL RECORD STANDARD
             VALUE OF FILE-ID W02-JOBDET.
       01  JEX-REC1.
    03  JEX-KEY.
            05  JEX-REF       PIC  X(08).
      05  JEX-SEQN.
   07  JEX-SEQ   PIC  9(03).
   07  JEX-SUB   PIC  9(02).
      *
      *    ****    TYPE : 1  - ITEM DETAILS
      *                 : 2  -  DISCOUNT DETAILS
      *                 : 3  -  REMARKS
      *                 : 4  -  INTERNAL NOTES FOR PICKING SLIP
      *   : 7  - REQUISITIONS
      *                 : 8  -  JOB REQUIRED
      *
    03  JEX-TYPE        PIC  9(01).
          03  JEX-DETAIL.
      05  JEX-EXT-ITEM.
         07  JEX-ITEM  PIC  X(14).
   07  JEX-ITM   PIC  X(03).
   07  FILLER    PIC  X(01).
            05  JEX-QNT       PIC S9(06)V999 COMP-3.
      05  JEX-COST      PIC S9(06)V999 COMP-3.
      05  JEX-SELL      PIC S9(08)V999 COMP-3.
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
       77  WS-JEXKEY    PIC 9(06)  COMP-5.
       77  WS-JEXLAST    PIC 9(06)  COMP-5.
       77  WS-PARKEY    PIC 9(06)  COMP-5.
       77  WS-USE-ITM    PIC X(01).
       77  WS-EXT-STK    PIC X(01).
       77  WS-JOB-STAT    PIC X(01) VALUE "Y".
    88 CLOSE-JOB    VALUE "Y".
       77  WS-OPTION       PIC  X(01).
       77  WS-ETYPE    PIC X(01).
       77  WS-SEQ    PIC X(01) VALUE "D".
       77  WS-ERROR        PIC  9(01) VALUE ZERO.
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "STPJOB".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "STOCK/INVENTORY -JOB DETAILS".
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
           03  WS-PROG         PIC  X(03) VALUE "POR".

       01  WS-PARID.
          03  WS-SYS-ID       PIC  X(03).

       01  W02-FID.
          03  W02-JOBDET.
              05  FILLER      PIC  X(04) VALUE "JOBX".
              05  FILLER      PIC  X(01) VALUE "_".
              05  W02-USE13T  PIC  X(03).
              05  FILLER      PIC  X(04) VALUE ".APC".

       01  W10-STOCK.
     03  W10-EXT-ITEM.
             05  W10-ITEM   PIC  X(14).
        05  W10-ITM    PIC  X(03).
        05  FILLER     PIC  X(01).
          03  W10-DATE.
        05  W10-EDTE   PIC  99/99/9999.
    03  W10-COST       PIC S9(07)V99 COMP-3.

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
    03  W100-Q1.
        05  W100-QNT PIC Z(05)9.999.

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

       01  W16-CALL.
    03  W16-EXT-ITEM.
        05  W16-ITEM  PIC  X(14).
        05  W16-ITM   PIC  X(03).
        05  FILLER    PIC  X(01).
    03  W16-CRED      PIC  X(06).
    03  W16-COST      PIC S9(07)V99 COMP-3.
    03  W16-CANCEL    PIC  X(01).

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

       COPY "BLANK.CRT".

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
      PERFORM JB-VIEW-QUANT.
    IF CLOSE-JOB
        CLOSE JOBCST.
      CLOSE JOBDET.

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

       COPY "JOBCST.RD".

      /
      *    ****    V I E W   J O B   C A R D   Q U A N T I T I E S
      *
      *          ÚÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿ 5
      *          ³ Job Ref³ Quantity ³°° 6
      *          ÃÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄ´°° 7
      *          ³JJJJJJJJ³ZZZZZ9.999³°° 8
      *          ³JJJJJJJJ³ZZZZZ9.999³°° 9
      *          ³JJJJJJJJ³ZZZZZ9.999³°° 10
      *          ³JJJJJJJJ³ZZZZZ9.999³°° 11
      *          ³JJJJJJJJ³ZZZZZ9.999³°° 12
      *          ³JJJJJJJJ³ZZZZZ9.999³°° 13
      *          ³JJJJJJJJ³ZZZZZ9.999³°° 14
      *          ³JJJJJJJJ³ZZZZZ9.999³°° 15
      *          ³JJJJJJJJ³ZZZZZ9.999³°° 16
      *          ³JJJJJJJJ³ZZZZZ9.999³°° 17
      *          ÀÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÙ°° 18
      *     °°°°°°°°°°°°°°°°°°°°°
      *
      *
       JB-VIEW-QUANT   SECTION 50.
       JB-VIEW.
      PERFORM SAVE-SCREEN.
      PERFORM CLEAR-L50.
      DISPLAY "ÚÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿" AT 0554
        WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
      DISPLAY "³ Job Ref³ Quantity ³" AT 0654
        WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
      DISPLAY "ÃÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄ´" AT 0754
        WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
      DISPLAY "ÀÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÙ" AT 1854
        WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
      MOVE 0854   TO CPOS.
      PERFORM JB-VIEW-CRT-EDGE UNTIL CLIN = 18.
      MOVE 6   TO SHADE-ROW.
      MOVE 56   TO SHADE-COL.
      MOVE 19   TO SHADE-WIDTH.
      MOVE 13   TO SHADE-LINES.
      PERFORM SCREEN-SHADOW.
      MOVE W16-EXT-ITEM  TO JOB-EXT-ITEM.
      MOVE 1   TO JOB-TYPE.
      MOVE SPACES  TO JOB-REF.

       JB-VIEW-START.
      PERFORM START-AT-JOB-DETAIL THRU READ-JOBCST-EXIT.
    IF WS-F-ERROR = 13
        GO TO JB-VIEW-RESTORE.
      MOVE ZERO   TO WS-JEXKEY.

       JB-EXTRACT-DET.
      PERFORM READ-JOBCST-NEXT THRU READ-JOBCST-EXIT.
    IF WS-F-ERROR = 13
        IF WS-JEXKEY > ZERO
     GO TO JB-STORE-END
        ELSE
     GO TO JB-NO-DETAIL.
    IF NOT (JOB-TYPE = 1)
        IF WS-JEXKEY > ZERO
     GO TO JB-STORE-END
        ELSE
     GO TO JB-NO-DETAIL.
    IF NOT (W16-EXT-ITEM = JOB-EXT-ITEM)
        IF WS-JEXKEY > ZERO
     GO TO JB-STORE-END
        ELSE
     GO TO JB-NO-DETAIL.
             MOVE JOB-REF  TO JEX-REF.
       MOVE JOB-SEQN  TO JEX-SEQN.
       MOVE JOB-TYPE  TO JEX-TYPE.
       MOVE JOB-EXT-ITEM  TO JEX-EXT-ITEM.
             MOVE JOB-QNT  TO JEX-QNT.
       MOVE JOB-COST  TO JEX-COST.
       MOVE JOB-SELL  TO JEX-SELL.
       ADD 1   TO WS-JEXKEY.
       WRITE JEX-REC1.
       GO TO JB-EXTRACT-DET.

       JB-STORE-END.
      MOVE WS-JEXKEY  TO WS-JEXLAST.
      MOVE 1   TO WS-JEXKEY.

       JB-VIEW-TOP.
            MOVE 08   TO CLIN.

       JB-VIEW-READ-NEXT.
      READ JOBDET.
          IF NOT (WS-STATUS = "00")
        GO TO JB-VIEW-USER-OPT.
      MOVE JEX-QNT  TO W100-QNT.
      DISPLAY (CLIN, 55) JEX-REF
        WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
      DISPLAY ( , 64) W100-Q1
        WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
    IF CLIN < 17
        IF WS-JEXKEY < WS-JEXLAST
     ADD 1  TO CLIN WS-JEXKEY
     GO TO JB-VIEW-READ-NEXT.

       JB-VIEW-USER-OPT.
            DISPLAY "PgUp" AT 5027
        WITH BACKGROUND-COLOR Cyan
      FOREGROUND-COLOR Brown HIGHLIGHT
       "/" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
       "PgDn" WITH BACKGROUND-COLOR Cyan
     FOREGROUND-COLOR Brown HIGHLIGHT
       " - " WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
             "Esc" WITH BACKGROUND-COLOR Cyan
    FOREGROUND-COLOR Brown HIGHLIGHT
       "ape to quit" WITH BACKGROUND-COLOR Cyan
     FOREGROUND-COLOR Blue.

       JB-VIEW-GET-OPT.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO JB-VIEW-RESTORE
   WHEN PAGE-DOWN
             GO TO JB-VIEW-CHK-END-CRT
   WHEN PAGE-UP
             GO TO JB-VIEW-READ-PREV
   WHEN OTHER  PERFORM AA900-ALARM
        END-EVALUATE
        GO TO JB-VIEW-GET-OPT.
      PERFORM AA900-ALARM.
            GO TO JB-VIEW-GET-OPT.

       JB-VIEW-CHK-END-CRT.
    IF CLIN < 17
        GO TO JB-VIEW-GET-OPT.

       JB-VIEW-CLEAR-CRT.
      MOVE 0854   TO CPOS.
      PERFORM JB-VIEW-CRT-EDGE UNTIL CLIN = 18.
            GO TO JB-VIEW-TOP.

       JB-VIEW-READ-PREV.
    IF WS-JEXKEY > 19
        SUBTRACT 20  FROM WS-JEXKEY
    ELSE
        MOVE 1   TO WS-JEXKEY.
      GO TO JB-VIEW-TOP.

       JB-NO-DETAIL.
      MOVE "No outstanding jobs"
     TO WS-ERR-MES
            PERFORM ERROR-MESSAGE
      GO TO JB-VIEW-RESTORE.

       JB-VIEW-CRT-EDGE.
      DISPLAY "³        ³          ³" AT CPOS
        WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
      ADD 1   TO CLIN.

       JB-VIEW-RESTORE.
             PERFORM RESTORE-SCREEN.

       JB-VIEW-EXIT.
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
            MOVE 3   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      MOVE PAR-USE-ITM  TO WS-USE-ITM.
      MOVE PAR-EXT-STK  TO WS-EXT-STK.
            OPEN I-O JOBCST.
    IF WS-STATUS = "41"
        MOVE "N"   TO WS-JOB-STAT
        GO TO ZA05.
          IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
           GO TO ZA49
               ELSE
        IF IDX-CORRUPT OR
   (WS-STATUS = "39")
     MOVE 13  TO WS-F-ERROR
     GO TO ZA50.
           IF WS-STATUS NOT = "00"
              MOVE 13   TO WS-F-ERROR
               PERFORM OPEN-ERROR.

       ZA05.
      OPEN OUTPUT JOBDET.
          IF RUNTIME-ERROR
              IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
           GO TO ZA49
        ELSE
           MOVE 32  TO WS-F-ERROR
              PERFORM OPEN-ERROR.
      CLOSE JOBDET.
      OPEN I-O JOBDET.
            GO TO ZA999-EXIT.

       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
            GO TO ZA205.
       ZA50.
          IF WS-F-ERROR = 13
              MOVE "JOB COSTING file"
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
          IF WS-F-ERROR = 13
              MOVE W02-JOBCST  TO WS-FILE
               MOVE ZERO         TO WS-KEY
        MOVE JOB-KEY  TO WS-KEYX
    ELSE
          IF WS-F-ERROR = 32
              MOVE W02-JOBDET  TO WS-FILE
              MOVE WS-JEXKEY  TO WS-KEY
        MOVE JEX-EXT-ITEM TO WS-KEYX
               WHEN OTHER MOVE "ERROR"      TO WS-FILE
                          MOVE "STPJOB"     TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
