      $set LINKCOUNT"512"
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
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     STPREAD.
000030 AUTHOR.         J.W.LEMMON (CSS).
000040 DATE-WRITTEN.   JANUARY 1982.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1982 - 2013
				     by James William Lemmon.
				       (Id No. 4412165050082).

		   All rights reserved.

		   e-mail jwlemmon@gmail.com.

000050 SECURITY.
		   This program is free software; you can redistribute
		   it and/or modify it under the terms of the GNU General
		   Public License as published by the Free Software
		   Foundation; either version 2 of the License, or (at
		   your option) any later version.

		   This program is distributed in the hope that it will
		   be useful, but WITHOUT ANY WARRANTY; without even the
		   implied warranty of MERCHANTABILITY or FITNESS FOR A
		   PARTICULAR PURPOSE.	See the GNU General Public License
		   for more details.

		   You should have received a copy of the GNU General
		   Public License along with this program; if not, write
		   to the Free Software Foundation, Inc., 59 Temple Place
		   - Suite 330, Boston, MA 02111-1307, USA.

000050 ENVIRONMENT DIVISION.
000060 CONFIGURATION SECTION.
000070 SPECIAL-NAMES.
000080                 CURSOR IS CSTART
000090                 CONSOLE IS CRT
                       CRT STATUS IS KEY-STATUS.
000090 INPUT-OUTPUT SECTION.
000100 FILE-CONTROL.

000120 COPY "DEPART.SL".

000120 COPY "PARAM.SL".

000140 COPY "STOCK.SL".

000140 COPY "WSTOCK.SL".

000250 DATA DIVISION.
000260 FILE SECTION.

       COPY "DEPART.FDE".

000280 COPY "PARAM.FDE".

000300 COPY "STOCK.FDE".

000300 COPY "WSTOCK.FDE".

      /
001690 WORKING-STORAGE SECTION.
       77  WS-CHECK                  PIC  X(18)    VALUE "aeWlimemnomLalismJ".
001710 77  WS-S1                     PIC  9(04)    COMP-5.
001720 77  WS-S2                     PIC  9(04)    COMP-5.
001730 77  WS-S3                     PIC  9(04)    COMP-5.
001740 77  WS-S4                     PIC  9(04)    COMP-5.
001860 77  WS-PARKEY                 PIC  9(06)    COMP-5.
       77  WS-OPTION                 PIC  X(01).
       77  WS-EXT-STK                PIC  X(01).

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Used by calling program (WORKING-STORAGE) and called program ³
      *    ³ (LINKAGE SECTION) for Main screen layout and headings.	  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       01  CRT-HEADINGS.
      *
      *    TYPE:
      * 	 C =  Clear comment line (50)
      *	 E =  Clear lines (any line or lines between 5 and 46)
      * 	 H =  Change heading
      * 	 S =  Clear the screen
      *
           03  CRT-TYPE              PIC  X(01).
      *
      *    The following two fields are used for clearing a portion of
      *    the screen. (Start line and end line)
      *
           03  CRT-START             PIC  9(02).
           03  CRT-END	             PIC  9(02).
      *
      *    Calling Program
      *
           03  CRT-PROGRAM           PIC  X(15) VALUE "STPREAD".
      *
      *    Screen Heading
      *
           03  CRT-HEADING           PIC  X(40) VALUE "STOCK/INVENTORY READ RECORD".
      *
      *    Company Name
      *
           03  CRT-COMP              PIC  X(40).
      *
      *    WorkStation / Supervisor
      *
           03  CRT-WRKHD             PIC  X(11).
           03  CRT-WRKST             PIC  X(03) VALUE "ÄÄÄ".

       COPY "FUNCTION.WS".

       COPY "WS.WS".

       COPY "W05.VAT".

       COPY "W05.RND".

       COPY "W40.WS".


003800 COPY "W50.WS".

       LINKAGE SECTION.

       77  LS-F-ERROR                PIC  9(02) COMP-5.
000010 77  W02-DEPART                PIC  X(64).
000010 77  W02-PARAM                 PIC  X(64).
       77  W02-STOCKF                PIC  X(64).
       77  W02-WSTOCK                PIC  X(64).
       77  W40-CODE                  PIC  X(06).

       COPY "CHAIN.LS".

       COPY "W10.STK".


001230 PROCEDURE DIVISION USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY W02-DEPART W02-PARAM W02-STOCKF W10-STOCK W02-WSTOCK LS-F-ERROR W40-CODE.
007740 AA000-MAIN	       SECTION.
       AA00.
007760       PERFORM ZA000-INIT.
      *
      *      LS-INSTR:
      *               A = Alternate Code
      *               I = Item Code
      *               X = X-Ref Code
      *
             EVALUATE LS-INSTR
               WHEN "A"
                    PERFORM CA100
               WHEN "I"
                    PERFORM CA000
               WHEN "X"
                    PERFORM CA200
               WHEN OTHER
                    MOVE "Invalid parameter passed to STPREAD" TO WS-ERR-MES
                    PERFORM ERROR-MESSAGE
                    STOP RUN
             END-EVALUATE.
             MOVE WS-F-ERROR         TO LS-F-ERROR.
             MOVE SPACE              TO LS-INSTR LS0-INSTR.

       AA99.
	     EXIT PROGRAM.


      /    *************************************************************
      *    ****    ROUTINES TO HANDLE VARIOUS FUNCTIONS FOR THE
      * 	   S C R E E N ,   K E Y B O A R D   &	 M O U S E
      *    *************************************************************
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	   SAVE-SCREEN /-2/-3  and  RESTORE-SCREEN /-2/-3    ³
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
      *    ³ CHECK-SETUP THRU CHECK-EXIT.	                             ³
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ****   T H I S   R O U T I N E   I S   U S E D   T O
      * 	       D I S P L A Y   E R R O R   M E S S A G E S
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

      /
      *    ****    READ FILES ROUTINES
      *
008180 AC000-READ              SECTION.

       COPY "DEPART.RD".

       COPY "PARAM.RD".

       COPY "STOCK.RD".

       COPY "WSTOCK.RD".

      *
      *    ****   G E T   S T O C K   R E C O R D   U S I N G   C O D E
      *
015170 CA000         SECTION.
015180 CA00.
013310       MOVE 18                 TO W50-LENGTH.

       CA05.
           IF WS-EXT-STK = "Y"
013320         MOVE W10-EXT-ITEM     TO W50-DATA
           ELSE
               MOVE W10-ITEM         TO W50-DATA
               MOVE W10-ITM          TO W50-ITM.
           IF W50-1 = SPACE
               IF WS-EXT-STK = "Y"
                   MOVE W50-17       TO W10-EXT-ITEM
               ELSE
                   MOVE W50-13       TO W10-ITEM
               END-IF
               GO TO CA05.
             CALL "CBL_TOUPPER" USING W50-DATA BY VALUE W50-LENGTH RETURNING W50-STATUS.
013380       MOVE W50-EXT-ITEM       TO W10-EXT-ITEM.

       CA06.
013380       MOVE W50-EXT-ITEM       TO STK-CODE.

       CA10.
             PERFORM READ-STOCK THRU READ-STOCK-EXIT.
           IF WS-STATUS = "23"
               IF LS0-INSTR = "X"
                   MOVE W50-EXT-ITEM TO W10-EXT-CODE2
                   PERFORM CA200
                   IF NOT(STK-XREF = W10-EXT-ITEM)
                       MOVE 22       TO WS-F-ERROR
                   ELSE
                       MOVE STK-CODE TO W10-EXT-ITEM.
           IF WS-F-ERROR = 0
               IF LS0-INSTR = "W"
                   PERFORM CA600
               ELSE
                   PERFORM CA500.

       CA99.
             EXIT.

      *
      *    ****   G E T   S T O C K   R E C O R D   U S I N G
      *           A L T E R N A T E   C O D E
      *
015170 CA100         SECTION.
015180 CA101.
013310       MOVE 10                 TO W50-LENGTH.
       CA105.
013320       MOVE W10-ACODE          TO W50-DATA.
           IF W50-1 = SPACE
               MOVE W50-9            TO W10-ACODE
               GO TO CA105.
             CALL "CBL_TOUPPER" USING W50-DATA BY VALUE W50-LENGTH RETURNING W50-STATUS.
013380       MOVE W50-ACODE          TO STK-ACODE W10-ACODE.
       CA110.
             PERFORM START-AT-ALT-CODE THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 22
               GO TO CA199.
             PERFORM READ-STOCK-NEXT THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 0
               IF LS0-INSTR = "W"
                   PERFORM CA600
               ELSE
                   PERFORM CA500.
       CA199.
             EXIT.

      *
      *    ****   G E T   S T O C K   R E C O R D   U S I N G   X R E F
      *
015170 CA200         SECTION.
015180 CA201.
013310       MOVE 18                 TO W50-LENGTH.

       CA205.
013320       MOVE W10-EXT-CODE2      TO W50-DATA.
           IF W50-1 = SPACE
               MOVE W50-17           TO W10-EXT-CODE2
               GO TO CA205.
             CALL "CBL_TOUPPER" USING W50-DATA BY VALUE W50-LENGTH RETURNING W50-STATUS.
013380       MOVE W50-EXT-ITEM       TO W10-EXT-CODE2.

       CA206.
013380       MOVE W50-EXT-ITEM       TO STK-XREF.

       CA210.
             PERFORM START-AT-STOCK-XREF THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 22
               GO TO CA299.
             PERFORM READ-STOCK-NEXT THRU READ-STOCK-EXIT.
	   IF NOT(STK-XREF = W50-EXT-ITEM)
	       GO TO CA299.
         IF LS0-INSTR = "W"
             PERFORM CA600
         ELSE
             PERFORM CA500.

       CA299.
             EXIT.

      *
      *    ****    Calculate the inclusive prices
      *
       CA500         SECTION.
       CA505.
      *
      * 	 Adjust the average cost for negative or zero quantity
      *
           IF STK-QUANT < 0.001
               MOVE STK-COST         TO STK-AVGE.
             MOVE 1                  TO W50-S1.

       CA510.
           IF STK-WSL(W50-S1) = ZERO
               IF STK-CSH(W50-S1) NOT = ZERO
                   MOVE STK-CSH(W50-S1)  TO STK-WSL(W50-S1)
               ELSE
                   MOVE STK-SL(W50-S1)   TO STK-WSL(W50-S1).
015630     IF STK-CSH(W50-S1) = ZERO
015640         MOVE STK-SL(W50-S1)   TO STK-CSH(W50-S1).
             MOVE STK-SL(W50-S1)     TO W10-SL(W50-S1).
             MOVE STK-CSH(W50-S1)    TO W10-CSH(W50-S1).
             MOVE STK-WSL(W50-S1)    TO W10-WSL(W50-S1).
             MOVE ZERO               TO W05-RATE.
           IF STK-TAX > 0
               MOVE W05-VAT(STK-TAX) TO W05-RATE.
             COMPUTE W10-VCSH(W50-S1) ROUNDED = W10-CSH(W50-S1) + (W10-CSH(W50-S1) * W05-RTE).
             COMPUTE W10-VWSL(W50-S1) ROUNDED = W10-WSL(W50-S1) + (W10-WSL(W50-S1) * W05-RTE).
             COMPUTE W10-VSL(W50-S1) ROUNDED = W10-SL(W50-S1) + (W10-SL(W50-S1) * W05-RTE).
           IF W50-S1 < 4
               ADD 1                 TO W50-S1
               GO TO CA510.
           IF STK-PRC = ZERO
               MOVE 1                TO STK-PRC.

015650 CA599.
015660       EXIT.

      *
      *    ****    Calculate the inclusive prices
      *
       CA600         SECTION.
       CA605.
      *
      * 	 Adjust the average cost for negative or zero quantity
      *
           IF STK-QUANT < 0.001
               MOVE STK-COST         TO STK-AVGE.
           IF STK-WSALE = ZERO
               IF STK-CASH NOT = ZERO
                   MOVE STK-CASH     TO STK-WSALE
               ELSE
                   MOVE STK-SELL     TO STK-WSALE.
015630     IF STK-CASH = ZERO
015640         MOVE STK-SELL         TO STK-CASH.
             MOVE STK-CASH           TO W10-CASH.
             MOVE STK-SELL           TO W10-SELL.
             MOVE STK-WSALE          TO W10-WSALE.
             MOVE ZERO               TO W05-RATE.
           IF STK-TAX > 0
               MOVE W05-VAT(STK-TAX) TO W05-RATE.
             COMPUTE W10-VCASH ROUNDED = W10-CASH + (W10-CASH * W05-RTE).
             COMPUTE W10-VWSALE ROUNDED = W10-WSALE + (W10-WSALE * W05-RTE).
             COMPUTE W10-VSELL ROUNDED = W10-SELL + (W10-SELL * W05-RTE).
           IF STK-PRC = ZERO
               MOVE "EA"             TO STK-PACK.
           IF (W40-CODE = SPACES) OR (STK-IND > 1)
               GO TO CA699.
             MOVE W40-CODE           TO WST-WAR.
             MOVE W10-ITEM           TO WST-CODE.
             MOVE SPACES             TO WST-ITM.
             PERFORM READ-WSTOCK THRU READ-WSTOCK-EXIT.
           IF WS-F-ERROR = 52
               MOVE ZERO             TO WST-QUANT.

015650 CA699.
015660       EXIT.

      /
044130 ZA000-INIT	       SECTION.
044140 ZA000-OPEN.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

	     COPY "FUNCTION.PRO".

       ZA00A.
           IF LS-USER = LS-SYS-ID
               MOVE "SupervisorÄ"    TO CRT-WRKHD
           ELSE
               MOVE "Workstation"    TO CRT-WRKHD
               MOVE LS-USER	       TO CRT-WRKST.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³             Get VAT rates and rounding details.           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
042520       MOVE 5                  TO WS-PARKEY.
042550       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
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

044870 ZA999-EXIT.
044880       EXIT.
      /
044890 ZB000-ERROR	       SECTION.

       COPY "ERRORS.PRO".

041560 DISPLAY-FILE-NAME.
041850     IF WS-F-ERROR = 15
041860         MOVE W02-PARAM        TO WS-FILE
041870         MOVE WS-PARKEY        TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 22
041860         MOVE W02-STOCKF       TO WS-FILE
041870         MOVE ZERO             TO WS-KEY
               MOVE STK-CODE         TO WS-KEYX.

       COPY "DISPERR.PRO".
