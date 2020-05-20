      $set LINKCOUNT"512" GNT"STPREAD.GNT"
      *
      *       ONLY DELETE WHEN ALL BELOW IS COMPLETED  
      *     ===========================================
      *
      *  Screen co-ordinates must be revised for new layout
      *
      *  Screen Section                            Completed  
      *                                               ___
      *                    Layout screen co-ordinates| x |
      *                                               ---
      *                                               ___
      *                    Input screen co-ordinates | x |
      *                                               ---
      *                                               ___
      *                    Output screen co-ordinates| x |
      *                                               ---
      *  Procedure Division 
      *                                               ___
      *                    Screen Heading Format     | x |
      *                                               ---
      *                                               ___
      *                    Error Messages            | x |
      *                                               ---
      *                                               ___
      *                    User information messages | x |
      *                                               ---
      *                                               ___
      *                    Accept User options       | x |
      *                                               ---
      *                                               ___
      *                    Display co-ordinates      | x |
      *                                               ---
      *                                               ___
      *                    Accept co-ordinates       | x |
      *                                               ---
      *  Accept and Display of Stk-code allows
      *                                               ___
      *         Display ITM, EXT and 14 Characters   | x |
      *                                               ---
      *                                               ___
      *         Accept ITM, EXT, 14 Characters and   | x |
      *            all the Numeric Formats 3 to 14    ---
      *
      *  Stock file
      *                                               ___       
      *         Use the new layout and W10.STK       | x |
      *                                               ---
      ******************************************************************
      *                                                                *
      *    ******  ******** *******  ******   ******   ***   ******    *
      *   **    **    **    **    ** **   **  **      ** **  **   **   *
      *   **          **    **    ** **   **  **     **   ** **   **   *
      *    ******     **    *******  *****    ****   ******* **   **   *
      *         **    **    **       **  **   **     **   ** **   **   *
      *   **    **    **    **       **   **  **     **   ** **   **   *
      *    ******     **    **       **   **  ****** **   ** ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      * R E A D   A   S T O C K   R E C O R D   U S I N G   A N Y  O F *
      * I T E M ,  A L T E R N A T E   O R  X R E F   C O D E S        *
      *                                                                *
      *     VERSION 9.00.00 - November 2012                            *
      *                                                                *
      ******************************************************************
      *                                                                *
      * This program is called by all programs that need to read a     *
      * stock record where a user has keyed in an Item, alternate or   *
      * xref code.                                                     *
      *   LS-INSTR is used to specify which code to use:               *
      *             A = Alternate Code                                 *
      *             I = Item Code                                      *
      *             S = Stock/Item or X-Ref Code                       *
      *             X = X-Ref Code                                     *
      *                                                                *
      * In addition to the above, LS0-INSTR is used to specify:        *
      *             W = Read Warehouse Stock as well.                  *
      *             X = If Item Code does not exist use the same code  *
      *                 as an X-Ref code (Used in Lookups)             *
      *                                                                *
      * The program will exit with the record available to the calling *
      * program or with an error code, indicating at e Item was not    *
      * found in the data base.                                        *
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
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     STPREAD.
       AUTHOR.         J.W.LEMMON.
       DATE-WRITTEN.   JANUARY 2012.

                COPYRIGHT NOTICE: COPYRIGHT (C) 2012 - 2018
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

       COPY "DEPART.SL".

       COPY "PARAM.SL".

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

       COPY "DEPART.FDE".

       COPY "PARAM.FDE".

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
       77  WS-CHECK                  PIC  X(18)           VALUE "aeWlimemnomLalismJ".
       77  WS-S1                     PIC  9(04)    COMP-5.
       77  WS-S2                     PIC  9(04)    COMP-5.
       77  WS-S3                     PIC  9(04)    COMP-5.
       77  WS-S4                     PIC  9(04)    COMP-5.
       77  WS-PARKEY                 PIC  9(06)    COMP-5.
       77  WS-OPTION                 PIC  X(01).
       77  WS-EXT-STK                PIC  X(01).

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Used by calling program (WORKING-STORAGE) and called program ³
      *    ³ (LINKAGE SECTION) for Main screen layout and headings.   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       01  CRT-HEADINGS.
      *
      *    TYPE:
      *         C = Clear comment line (50)
      *         E = Clear lines (any line or lines between 5 and 46)
      *         H = Change heading
      *         S = Clear the screen and display headings
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "STPREAD".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "STOCK/INVENTORY READ RECORD".
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

       COPY "FUNCTION.WS".

       COPY "WS.WS".

       COPY "W05.VAT".

       COPY "W05.RND".

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

       77  LS-F-ERROR                PIC  9(02) COMP-5.
       77  WS-EXT-STK                PIC  X(01).

       COPY "CHAIN.LS".

       COPY "FILE.IDS".

       COPY "W10.STK".

      *
      *      ******   ******    *****    *****   ******  ******   **   **  ******    ****** 
      *      **   **  **   **  **   **  **   **  **      **   **  **   **  **   **   **
      *      **   **  **  **   **   **  **       **      **   **  **   **  **  **    **
      *      ******   *****    **   **  **       *****   **   **  **   **  *****     *****
      *      **       **  **   **   **  **       **      **   **  **   **  **  **    **
      *      **       **   **  **   **  **   **  **      **   **  **   **  **   **   **
      *      **       **   **   *****    *****   ******  ******    *****   **   **   ******
      *
       PROCEDURE DIVISION USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY DATA-FILE-IDS W10-STOCK WS-EXT-STK LS-F-ERROR.
       AA000-MAIN        SECTION.
       AA00.
             PERFORM SAVE-SCREEN-3.
             PERFORM ZA000-INIT.
      *
      *      W10-INSTR:
      *               A = Alternate Code
      *               I = Item Code
      *               S = Stock/Item or X-Ref Code
      *               X = X-Ref Code
      *
             EVALUATE W10-INSTR
               WHEN "A"   PERFORM CA100
               WHEN "I"   PERFORM CA000
               WHEN "S"   PERFORM CA000
               WHEN "X"   PERFORM CA200
               WHEN OTHER
                    MOVE "Invalid parameter passed to STPREAD" TO WS-ERR-MES
                    PERFORM ERROR-MESSAGE
                    STOP RUN
             END-EVALUATE.
           IF WS-F-ERROR = ZERO
               MOVE STK-CODE         TO W10-EXT-ITEM.
             MOVE WS-F-ERROR         TO LS-F-ERROR.
             MOVE SPACE              TO LS-INSTR LS0-INSTR.
             PERFORM RESTORE-SCREEN-3.

       AA99.
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

       COPY "DATE.CNV".

      *
      *    ****    R E A D   F I L E S   R O U T I N E S
      *
       AC000-READ              SECTION.

       COPY "DEPART.RD".

       COPY "PARAM.RD".

       COPY "STOCK.RD".

       COPY "WSTOCK.RD".

      *
      *    ****   G E T   S T O C K   R E C O R D   U S I N G   C O D E
      *
       CA000         SECTION.
       CA00.
             MOVE 18                 TO W50-LENGTH.

       CA05.
           IF WS-EXT-STK = "Y"
               MOVE W10-EXT-ITEM     TO W50-DATA
           ELSE
               MOVE W10-ITEM         TO W50-DATA
               MOVE W10-ITM          TO W50-ITM.
      *
      *    Remove leading spaces
      *         
           IF W50-1 = SPACE
               IF WS-EXT-STK = "Y"
                   MOVE W50-17       TO W10-EXT-ITEM
               ELSE
                   MOVE W50-13       TO W10-ITEM
               END-IF
               GO TO CA05.
             CALL "CBL_TOUPPER" USING W50-DATA BY VALUE W50-LENGTH RETURNING W50-STATUS.
             MOVE W50-EXT-ITEM       TO W10-EXT-ITEM.

       CA06.
             MOVE W50-EXT-ITEM       TO STK-CODE.

       CA10.
             PERFORM READ-STOCK THRU READ-STOCK-EXIT.
           IF (WS-STATUS = "23") AND (W10-INSTR = "S")
               MOVE W50-EXT-ITEM     TO W10-EXT-CODE2
               PERFORM CA200
               IF NOT(STK-XREF = W10-EXT-ITEM)
                   MOVE 22           TO WS-F-ERROR
               ELSE
                   MOVE STK-CODE     TO W10-EXT-ITEM.
           IF WS-F-ERROR = 0
               PERFORM CA500.

       CA99.
             EXIT.

      *
      *    ****   G E T   S T O C K   R E C O R D   U S I N G
      *                   A L T E R N A T E   C O D E
      *
       CA100         SECTION.
       CA101.
             MOVE 10                 TO W50-LENGTH.

       CA105.
             MOVE W10-ACODE          TO W50-DATA.
      *
      *    Remove leading spaces
      *         
           IF W50-1 = SPACE
               MOVE W50-9            TO W10-ACODE
               GO TO CA105.
             CALL "CBL_TOUPPER" USING W50-DATA BY VALUE W50-LENGTH RETURNING W50-STATUS.
             MOVE W50-ACODE          TO STK-ACODE W10-ACODE.

       CA110.
             PERFORM START-AT-ALT-CODE THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 22
               GO TO CA199.
             PERFORM READ-STOCK-NEXT THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 0
               PERFORM CA500.

       CA199.
             EXIT.

      *
      *    ****   G E T   S T O C K   R E C O R D   U S I N G   X R E F
      *
       CA200         SECTION.
       CA201.
             MOVE 18                 TO W50-LENGTH.

       CA205.
             MOVE W10-EXT-CODE2      TO W50-DATA.
      *
      *    Remove leading spaces
      *         
           IF W50-1 = SPACE
               MOVE W50-17           TO W10-EXT-CODE2
               GO TO CA205.
             CALL "CBL_TOUPPER" USING W50-DATA BY VALUE W50-LENGTH RETURNING W50-STATUS.
             MOVE W50-EXT-ITEM       TO W10-EXT-CODE2.

       CA206.
             MOVE W50-EXT-ITEM       TO STK-XREF.

       CA210.
             PERFORM START-AT-STOCK-XREF THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 22
               GO TO CA299.
             PERFORM READ-STOCK-NEXT THRU READ-STOCK-EXIT.
           IF NOT(STK-XREF = W50-EXT-ITEM)
               GO TO CA299.
             PERFORM CA500.

       CA299.
             EXIT.

      *
      *    ****    Calculate the inclusive prices
      *
       CA500         SECTION.
       CA505.
      *
      *   Adjust the average cost for negative or zero quantity
      *
           IF STK-QUANT < 0.001
               MOVE STK-COST         TO STK-AVGE.
             MOVE 1                  TO W50-S1.

       CA510.
      *
      *    If any of the cash or wholesale prices are zero insert the
      *    price from the higher price (for cash - retail and for 
      *                                   wholesale - cash)
      *
           IF STK-WSL(W50-S1) = ZERO
               IF STK-CSH(W50-S1) NOT = ZERO
                   MOVE STK-CSH(W50-S1)  TO STK-WSL(W50-S1)
               ELSE
                   MOVE STK-SL(W50-S1)   TO STK-WSL(W50-S1).
           IF STK-CSH(W50-S1) = ZERO
               MOVE STK-SL(W50-S1)   TO STK-CSH(W50-S1).
             MOVE STK-SL(W50-S1)     TO W10-SL(W50-S1).
             MOVE STK-CSH(W50-S1)    TO W10-CSH(W50-S1).
             MOVE STK-WSL(W50-S1)    TO W10-WSL(W50-S1).
             MOVE ZERO               TO W05-RATE.
           IF STK-TAX > 0
               MOVE W05-VAT(STK-TAX) TO W05-RATE.
             COMPUTE W10-VCSH(W50-S1) ROUNDED = W10-CSH(W50-S1) + (W10-CSH(W50-S1) * W05-RTE).
             COMPUTE W10-VWSL(W50-S1) ROUNDED = W10-WSL(W50-S1) + (W10-WSL(W50-S1) * W05-RTE).
             COMPUTE W10-VSL(W50-S1)  ROUNDED = W10-SL(W50-S1)  + (W10-SL(W50-S1)  * W05-RTE).
           IF W50-S1 < 4
               ADD 1                 TO W50-S1
               GO TO CA510.
           IF STK-PRC = ZERO
               MOVE 1                TO STK-PRC.
      *
      *    Switch the dates to DDMMCCYY format for display
      *         
             MOVE W10-DTE            TO W22-DTE2.
             PERFORM SWITCH-DATE-TO-DMCY THRU SWITCH-DATE-EXIT.
             MOVE W22-DTE3           TO W10-ODTE.
             MOVE W10-PUR            TO W22-DTE2.
             PERFORM SWITCH-DATE-TO-DMCY THRU SWITCH-DATE-EXIT.
             MOVE W22-DTE3           TO W10-PURCH.
      *
      *    Last active date used by some programs as edited
      *             
           IF STK-ACT = ZERO
               MOVE SPACE            TO W10-ACTIVE
           ELSE
               MOVE STK-ACT          TO W10-ADATE.
      *
      *    Move the record that has been read to the work area.
      *         
             MOVE STK-RECORD1        TO W10-STK-RECORD.  

       CA599.
             EXIT.

      *    *************************************************************
      *                I N I T I A L I S E   P R O G R A M
      *    *************************************************************
       ZA000-INIT        SECTION.
       ZA000-OPEN.
             MOVE LS-COMPANY         TO CRT-COMPANY.
             MOVE LS-TERMINAL        TO CRT-TERMINAL.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

      COPY "FUNCTION.PRO".

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³             Get VAT rates and rounding details.           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
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
             MOVE 1                  TO W05-V-RATE.

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
               WHEN 15    MOVE WS-PARID     TO WS-FILE
                          MOVE WS-PARKEY    TO WS-KEY
               WHEN 22    MOVE W02-STOCKF   TO WS-FILE
                          MOVE STK-CODE     TO WS-KEYX
               WHEN OTHER MOVE "ERROR"      TO WS-FILE
                          MOVE "STPREAD"    TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
