      $set LINKCOUNT"576"
      ******************************************************************
      *                                                                *
      *   ******   **	     *******   **     **  ********  **	   **  *
      *  **    **  **	     **	   **  ***   ***  **	    ***	   **  *
      *  **	   **	     **	   **  ** * * **  **	    ** *   **  *
      *   ******   **	     *******   **  *  **  *****	    **	*  **  *
      *        **  **	     **	       **     **  **	    **	 * **  *
      *  **    **  **	     **	       **     **  **	    **	  ***  *
      *   ******   ********  **        **     **  *******   **	   **  *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     S A L E S	A N D	O R D E R   M O D U L E   M E N U      *
      *                                                                *
      *     VERSION 8.15.02 - November 2011			       *
      * 							       *
      ******************************************************************
      * 							       *
      *  Jan 2008	- New file (DEBALT) - Debtor alternate index   *
      * 		  included for lookups, using any word con-    *
      * 		  tained in the Debtor name.		       *
      * 							       *
      *  Aug 2008	- New file (STKALT) - Stock alternate index    *
      * 		  included for lookups, using any word con-    *
      * 		  tained in the Stock description.	       *
      * 							       *
      * November 2009	- Include words from Description 2 and from    *
      * 		  the Item code (Some item codes are comprised *
      * 		  of individual words and these will be        *
      * 		  included in the alternate Index)	       *
      * 							       *
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		    VERSIONS OF THE APAC SYSTEM		       ³
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ³ While developing this system the requirements and budgets ³
      *    ³ (originally this was a propriety package) for various     ³
      *    ³ businesses necessitated that at least 4 separate systems  ³
      *    ³ be produced. As the maintenance and amendments to the 4   ³
      *    ³ different systems would require man hours that would not  ³
      *    ³ enable me to do much development, it was decided to use   ³
      *    ³ one system with the various requirements controlled by    ³
      *    ³ the various menu programs using a version indicator. This ³
      *    ³ indicator is located in the second record of the SYSTEM   ³
      *    ³ file SEC-LEV, which is in the SEC-MODULES. This indicator ³
      *    ³ together with all the other indicators in SEC-MODULES, is ³
      *    ³ moved to WS0-MODULES in BUP000 or BUP001 and then passed  ³
      *    ³ via the LINKAGE SECTION from program to program in  the   ³
      *    ³ LS0-MODULES group. The other indicators in this group are ³
      *    ³ used to show which modules of the system are installed.   ³
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ³	SYSTEM FILE    ³      CHAIN.WS	   ³	 CHAIN.LS      ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *	   ³  03  SEC-MODULES. ³  03  WS0-MODULES. ³  03  LS0-MODULES. ³
      *    ³	  05  SEC-DTP  ³      05  WS0-DTP  ³	  05  LS0-DTP  ³
      *    ³	  05  SEC-STP  ³      05  WS0-STP  ³	  05  LS0-STP  ³
      *    ³	  05  SEC-HPD  ³      05  WS0-HPD  ³	  05  LS0-HPD  ³
      *    ³	  05  SEC-CRP  ³      05  WS0-CRP  ³	  05  LS0-CRP  ³
      *    ³	  05  SEC-GLP  ³      05  WS0-GLP  ³	  05  LS0-GLP  ³
      *    ³	  05  SEC-POS  ³      05  WS0-POS  ³	  05  LS0-POS  ³
      *    ³	  05  SEC-JCP  ³      05  WS0-JCP  ³	  05  LS0-JCP  ³
      *    ³	  05  SEC-VHP  ³      05  WS0-VHP  ³	  05  LS0-VHP  ³
      *    ³	  05  SEC-WAG  ³      05  WS0-WAG  ³	  05  LS0-WAG  ³
      *    ³	  05  SEC-SAL  ³      05  WS0-SAL  ³	  05  LS0-SAL  ³
      *    ³	  05  SEC-LEV  ³      05  WS0-LEV  ³	  05  LS0-LEV  ³
      *    ³	  05  SEC-WHS  ³      05  WS0-WHS  ³	  05  LS0-WHS  ³
      *    ³	  05  SEC-RTE  ³      05  WS0-RTE  ³	  05  LS0-RTE  ³
      *	   ³	  05  FILLER   ³		   ³		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      *
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     SLPMENU.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   AUGUST 1991.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1991 - 2012
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
000090		       CONSOLE IS CRT
		       CRT STATUS IS KEY-STATUS.
000100 INPUT-OUTPUT SECTION.
000110 FILE-CONTROL.

       COPY APACFIDS.SL.

000130 COPY AUDIT.SL.

       COPY CARDEX.SL.

       COPY CREDIT.SL.

000170 COPY DBTRAN.SL.

000160 COPY DEBALT.SL.

000190 COPY DEBTOR.ISL.

000140 COPY DEBMEM.SL.

000250 COPY GARTEE.SL.

000250 COPY INVOIC.SL.

       COPY LEDTRF.SL.

       COPY ROUTE.SL.

000270 COPY SORDER.SL.

000330 COPY SPARTS.SL.

       COPY STKALT.SL.

000330 COPY STOCK.SL.

000140 COPY STKMEM.SL.

       COPY WARHSE.SL.

       COPY WSTOCK.SL.

      /
000330 DATA DIVISION.
000340 FILE SECTION.

       COPY APACFIDS.FDE.

000420 COPY AUDIT.FDE.

000160 COPY CARDEX.FDE.

000160 COPY CREDIT.FDE.

000460 COPY DBTRAN.FDE.

000330 COPY DEBALT.FDE.

000480 COPY DEBTOR.FDE.

000140 COPY DEBMEM.FDE.

000250 COPY GARTEE.FDE.

000540 COPY INVOIC.FDE.

       COPY LEDTRF.FDE.

       COPY ROUTE.FDE.

000500 COPY SORDER.FDE.

000620 COPY SPARTS.FDE.

       COPY STKALT.FDE.

000620 COPY STOCK.FDE.

000140 COPY STKMEM.FDE.

       COPY WARHSE.FDE.

       COPY WSTOCK.FDE.

      /
000560 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
000610 77  WS-S1	   PIC	9(04)	 COMP-5.
000620 77  WS-S2           PIC  9(04)    COMP-5.
000630 77  WS-S3           PIC  9(04)    COMP-5.
000640 77  WS-S4           PIC  9(04)    COMP-5.
       77  WS-AUDKEY	   PIC	9(06)	 COMP-5.
       77  WS-TEMP-LIN	   PIC	9(02)	 COMP-5.
       77  WS-OPTION	   PIC	X(01).
       77  PRG-NAME	   PIC	X(12) VALUE SPACES.
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.

001350 01  WS-CRT-LINES.
001380	   03  WS-TOP-LNE2.
001390	       05  WS-TCR   PIC  X(80) VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
001400	   03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
001410         05  FILLER      PIC  X(01).
001420         05  WS-TOP-COMP PIC  X(40).
001430	       05  FILLER      PIC  X(23).
	       05  WS-WRKHD    PIC  X(11).
001430	       05  FILLER      PIC  X(01).
	       05  WS-WRKST    PIC  X(03).
001430	       05  FILLER      PIC  X(01).
001460	   03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "³".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "°".
	       05  FILLER      PIC  X(01) VALUE "³".

001840 COPY WS.WS.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002040 01  W02-FID.

       COPY APACFIDS.ID.

003680 COPY AUDIT.ID.

000160 COPY CARDEX.ID.

000160 COPY CREDIT.ID.

       COPY DBTRAN.ID.

000330 COPY DEBALT.ID.

       COPY DEBTOR.ID.

000140 COPY DEBMEM.ID.

       COPY GARTEE.ID.

       COPY INVOIC.ID.

       COPY LEDTRF.ID.

       COPY PARAM.ID.

       COPY ROUTE.ID.

002180 COPY SORDER.ID.

       COPY SPARTS.ID.

       COPY STKALT.ID.

000140 COPY STKMEM.ID.

002220 COPY STOCK.ID.

       COPY WARHSE.ID.

       COPY WSTOCK.ID.

       01  BLOCK-DETAIL1.
	   03  ORIGINAL-VID1.
	       05  ORIGINAL-CHAR1 PIC  X(01) OCCURS 80.
	   03  REVERSE-VID1.
	       05  REVERSE-CHAR1  PIC  X(01) OCCURS 80.

       01  SHADOW-DETAIL1.
	   03  SHAD1-ROW       PIC  9(02) COMP-X.
	   03  SHAD1-COL       PIC  9(02) COMP-X.
	   03  SHAD1-LINES     PIC  9(02) COMP-X.
	   03  SHAD1-WIDTH     PIC  9(02) COMP-X.
	   03  SHAD1-CHAR      PIC  X(01).

       01  CRT-DETAIL1.
	   03  TOP-ROW1       PIC  9(02) COMP-X.
	   03  BOTTOM-ROW1     PIC  9(02) COMP-X.
	   03  STRING-LENGTH1  PIC  9(04) COMP-X.
	   03  SCREEN-POSITION1.
	       05  SCREEN-LIN1 PIC  9(02) COMP-X.
	       05  SCREEN-COL1 PIC  9(02) COMP-X.

       01  BLOCK-DETAIL2.
	   03  ORIGINAL-VID2.
	       05  ORIGINAL-CHAR2 PIC  X(01) OCCURS 80.
	   03  REVERSE-VID2.
	       05  REVERSE-CHAR2  PIC  X(01) OCCURS 80.

       01  SHADOW-DETAIL2.
	   03  SHAD2-ROW       PIC  9(02) COMP-X.
	   03  SHAD2-COL       PIC  9(02) COMP-X.
	   03  SHAD2-LINES     PIC  9(02) COMP-X.
	   03  SHAD2-WIDTH     PIC  9(02) COMP-X.
	   03  SHAD2-CHAR      PIC  X(01).

       01  CRT-DETAIL2.
	   03  TOP-ROW2       PIC  9(02) COMP-X.
	   03  BOTTOM-ROW2     PIC  9(02) COMP-X.
	   03  STRING-LENGTH2  PIC  9(04) COMP-X.
	   03  SCREEN-POSITION2.
	       05  SCREEN-LIN2 PIC  9(02) COMP-X.
	       05  SCREEN-COL2 PIC  9(02) COMP-X.

       COPY W40.WS.

       COPY FUNCTION.WS.

004540 COPY W50.WS.

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
000750 SCREEN SECTION.

       COPY BLANK.CRT.

       01  MENU-INSTRUCT.
	   03  LINE 25 COLUMN  2 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
				 VALUE "Use ".
	   03	       COLUMN  6 HIGHLIGHT
				 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 6
				 VALUE "".
	   03	       COLUMN  7 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
				 VALUE " or ".
	   03	       COLUMN 11 HIGHLIGHT
				 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 6
				 VALUE "".
	   03	       COLUMN 12 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
				 VALUE " to point to an item, and ".
	   03	       COLUMN 38 HIGHLIGHT
				 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 6
				 VALUE "<Enter>".
	   03	       COLUMN 45 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
				 VALUE " to Select ".
	   03	       COLUMN 56 HIGHLIGHT
				 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 6
				 VALUE "<Esc>".
	   03	       COLUMN 61 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
				 VALUE " to Exit".

       01  S01.
001450	   03  LINE  2 COLUMN 31 BACKGROUND-COLOR 1
				 FOREGROUND-COLOR 7 HIGHLIGHT
001460				 VALUE "SALES CONTROL MODULE".

       01  S02.
	   03  LINE  4 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
	   03	       COLUMN 28 BACKGROUND-COLOR 2 FOREGROUND-COLOR 0
				 VALUE "¿".
	   03  LINE  5 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³".
	   03	       COLUMN  8 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "SALES/ORDER PROGRAMS".
	   03	       COLUMN 28 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "³".
	   03  LINE  6 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "1".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Quotations       ³".
001470	   03  LINE  7 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "2".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Orders (Sales)   ³".
001470	   03  LINE  8 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "3".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Standing Orders  ³".
001470	   03  LINE  9 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "4".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Delivery Vehicles³".
001570	   03  LINE 10 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "5".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Proc. Deliveries ³".
001570	   03  LINE 11 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "6".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Invoice/Cr-Note  ³".
001570	   03  LINE 12 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "7".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Cash Sales (POS) ³".
001570	   03  LINE 13 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "8".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Debtor Receipts  ³".
001570	   03  LINE 14 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "A".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Guarantee Module ³".
001570	   03  LINE 15 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "B".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Enquiries        ³".
001570	   03  LINE 16 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "C".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Stock Order      ³".
001570	   03  LINE 17 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "D".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Sales Statistics ³".
001570	   03  LINE 18 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "E".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Sales Staff Trns ³".
001570	   03  LINE 19 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "F".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Sales per Ledger ³".
001570	   03  LINE 20 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "G".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Bar Code Labels  ³".
001500	   03  LINE 21 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "0".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Exit	          ³".
	   03  LINE 22 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "À".
	   03	       COLUMN  8 BACKGROUND-COLOR 2 FOREGROUND-COLOR 0
				 VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       COPY ERROR.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
       AA000	       SECTION.
       AA00.
	     PERFORM ZA000-INIT.
	     DISPLAY CLR-L1-2.
	     DISPLAY S01.
003240	   IF LS0-STP NOT = 2
	       MOVE "Not Installed"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	   ELSE
003240	   IF LS0-SALES = "N"
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	   ELSE
	       PERFORM BA000.
	   IF MOUSE-ATTACHED = "Y"
	       MOVE 66		 TO MOUSE-FUNC
	       MOVE 0		 TO MOUSE-PARAM
	       CALL X"AF" USING MOUSE-FUNC
				MOUSE-PARAM.
043610	     CLOSE AUDIT
		   CARDEX
		   CREDIT
013170		   DEBALT
		   DEBTOR
                   DEBMEM
043630             DBTRAN
		   GARTEE
		   INVOICE
043660		   LEDTRF
		   ROUTE
		   SORDER
		   SPARTS
		   STKALT
		   STKMEM
		   STOCK
		   WARHSE
		   WSTOCK.

013140 AA99.
013150       EXIT PROGRAM.

       AA100	       SECTION.
       AA101.
	   IF MOUSE-ATTACHED = "Y"
	       MOVE 66		 TO MOUSE-FUNC
	       MOVE 0		 TO MOUSE-PARAM
	       CALL X"AF" USING MOUSE-FUNC
				MOUSE-PARAM.
	     PERFORM SAVE-SCREEN-3.
	     CALL PRG-NAME
		  USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY
		  ON EXCEPTION
		      GO TO AA110
	     END-CALL.
	     CANCEL PRG-NAME.

       AA105.
	     PERFORM RESTORE-SCREEN-3.
	   IF MOUSE-ATTACHED = "Y"
	       MOVE 66		 TO MOUSE-FUNC
	       MOVE 1		 TO MOUSE-PARAM
	       CALL X"AF" USING MOUSE-FUNC
				MOUSE-PARAM.
	     GO TO AA199.
       AA110.
	     MOVE SPACE 	 TO WS-ERR-MES.
	     STRING "Program- " DELIMITED SIZE
		     PRG-NAME DELIMITED " "
		     " not on disk, press ANY key"
		     DELIMITED SIZE
		     INTO WS-ERR-MES.
	     MOVE W43-SCREEN	 TO W43-SCREEN2.
	     MOVE W42-ATTRIB	 TO W42-ATTRIB2.
	     PERFORM ERROR-LENGTH THRU ERROR-EXIT.
	     MOVE W43-SCREEN2	 TO W43-SCREEN.
	     MOVE W42-ATTRIB2	 TO W42-ATTRIB.
	     GO TO AA105.

       AA199.
	     EXIT.

      /    *************************************************************
      *    ****    ROUTINES TO HANDLE VARIOUS FUNCTIONS FOR THE
      * 	   S C R E E N ,   K E Y B O A R D   &	 M O U S E
      *    *************************************************************
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	   SAVE-SCREEN /-2/-3  and  RESTORE-SCREEN /-2/-3      ³
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ³			  SCREEN-SHADOW 		       ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ To routine is used to display a shadow down the right and ³
      *    ³ along the bottom of a pop-up box. The parameters that are ³
      *    ³ required:						       ³
      *	   ³	       SHADE-ROW   - Top line of the box + 1.	       ³
      *	   ³	       SHADE-COL   - Left line of box + 2.	       ³
      *	   ³	       SHADE-WIDTH - Width of the box - 2.	       ³
      *	   ³	       SHADE-LINES - Hight of box - 1.		       ³
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ³			  ERROR-MESSAGE 		       ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ To display an error message with " - Press ANY key" at    ³
      *    ³ the end; use PERFORM ERROR-MESSAGE.		       ³
      *    ³							       ³
      *    ³ To display just the error message; use PERFORM	       ³
      *    ³ ERROR-LENGTH THRU ERROR-EXIT.			       ³
      *    ³							       ³
      *    ³ To display the error message higher or lower (default is  ³
      *    ³ line 13) firstly use PERFORM ERROR-SETUP THRU ERROR-POS   ³
      *    ³ or PERFORM ERROR-LENGTH THRU ERROR-POS. Move the line     ³
      *    ³ number to be used to SLIN and then PERFORM ERROR-DISPLAY  ³
      *    ³ THRU ERROR-EXIT.					       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY FUNCTION.SCR.

      /    *************************************************************
      *    ****   T H I S   R O U T I N E   I S   U S E D   T O
      * 	  D I S P L A Y   E R R O R   M E S S A G E S
      *    *************************************************************
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³			  ERROR-MESSAGE 		       ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ To display an error message with " - Press ANY key" at    ³
      *    ³ the end; use PERFORM ERROR-MESSAGE.		       ³
      *    ³							       ³
      *    ³ To display just the error message; use PERFORM	       ³
      *    ³ ERROR-LENGTH THRU ERROR-EXIT.			       ³
      *    ³							       ³
      *    ³ To display the error message higher or lower (default is  ³
      *    ³ line 13) firstly use PERFORM ERROR-SETUP THRU ERROR-POS   ³
      *    ³ or PERFORM ERROR-LENGTH THRU ERROR-POS. Move the line     ³
      *    ³ number to be used to SLIN and then PERFORM ERROR-DISPLAY  ³
      *    ³ THRU ERROR-EXIT.					       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY ERROR.SC2.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	      ERASE SCREEN FROM SPECIFIED LOCATION	       ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ Uses CPOS (CLIN,CCOL) as starting position and increases  ³
      *    ³ CLIN by 1 until CLIN reaches the line allocated to exit.  ³
      *    ³ The screen is cleared with Column 1 and 80 containing "³" ³
      *    ³ and columns 2 to 79 containing spaces.		       ³
      *    ³  eg.						       ³
      *    ³	  MOVE 0801	 TO CPOS.			       ³
      *    ³	  PERFORM ERASE-SCREEN-LOOP UNTIL CLIN > 19.	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

011433 COPY CLEAR.CRT.

       BA000	       SECTION.
       BA00.
	     MOVE 5		 TO SHADE-ROW.
	     MOVE 9		 TO SHADE-COL.
	     MOVE 20		 TO SHADE-WIDTH.
	     MOVE 18		 TO SHADE-LINES.
	     DISPLAY S02.
	   IF LS0-LEV < 2
	       DISPLAY " (N/A)" AT 1721
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
	       DISPLAY ".N/A" AT 1924
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
	   IF LS0-LEV < 3
	       DISPLAY "(N/A)" AT 0622
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
	       DISPLAY ") (N/A)" AT 0720
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
	       DISPLAY "s (N/A) " AT 1420
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
	       DISPLAY "(N/A)" AT 1623
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
	       DISPLAY "(N/A)" AT 1823
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
	       DISPLAY "(N/A)" AT 2023
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2.
	   IF LS0-POS = 0
	       DISPLAY "N/A" AT 1223
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2.
	   IF (LS0-LEV < 4) OR (LS0-RTE = 0)
	       DISPLAY "(N/A)" AT 0823
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
	       DISPLAY "(N/A)" AT 0923
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
	       DISPLAY ".(N/A)" AT 1022
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.
       BA02.
	     MOVE 5		 TO TOP-ROW.
	     MOVE 20		 TO BOTTOM-ROW.
	     MOVE 4		 TO SCREEN-LIN.
	     MOVE 9		 TO SCREEN-COL.
	     MOVE 18		 TO STRING-LENGTH.
	     PERFORM SAVE-ATTR.

       BA05.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       BA10.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY	 PERFORM MOVE-BLOCK-UP
		 WHEN DOWN-KEY	 PERFORM MOVE-BLOCK-DOWN
		 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
				 IF MOUSE-ENTER = "Y"
				     MOVE "N"	 TO MOUSE-ENTER
				     GO TO BA15
				 END-IF
				 IF MOUSE-Y = 24
				     IF MOUSE-X = 5
					 MOVE 5  TO KEY-CODE-1
					 GO TO BA10
				     ELSE
				     IF MOUSE-X = 10
					 MOVE 6  TO KEY-CODE-1
					 GO TO BA10
				     ELSE
				     IF MOUSE-X > 36 AND < 44
					 MOVE 0  TO KEY-CODE-1
					 GO TO BA10
				     ELSE
				     IF MOUSE-X > 54 AND < 60
					 MOVE 0  TO KEY-CODE-1
					 MOVE 1  TO KEY-TYPE
					 GO TO BA10
				     END-IF
				 END-IF
		 WHEN ENTER-KEY  GO TO BA15
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BA05
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE "0"	 TO WS-OPTION
		     GO TO BA18
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BA05
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
	       IF (WS-OPTION < 0 OR > 8) AND
		  (WS-OPTION < "A" OR > "H") AND
		  NOT(WS-OPTION = ">" OR "@")
		   CALL X"E5"
		   GO TO BA05
	       END-IF
	       GO TO BA16
	   ELSE
	       CALL X"E5"
	       GO TO BA05.

       BA15.
	   IF SCREEN-LIN < 5
	       CALL X"E5"
	       GO TO BA05
	   ELSE
	   IF SCREEN-LIN = 5
	       MOVE "1"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 6
	       MOVE "2"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 7
	       MOVE "3"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 8
	       MOVE "4"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 9
	       MOVE "5"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 10
	       MOVE "6"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 11
	       MOVE "7"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 12
	       MOVE "8"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 13
	       MOVE "A"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 14
	       MOVE "B"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 15
	       MOVE "C"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 16
	       MOVE "D"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 17
	       MOVE "E"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 18
	       MOVE "F"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 19
	       MOVE "G"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 20
	       MOVE "0"		 TO WS-OPTION.
	   GO TO BA18.

       BA16.
	     PERFORM CLEAR-BLOCK.
	   IF WS-OPTION = "0" OR ">" OR "H" OR "@"
	       MOVE 20		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "1"
	       MOVE 5		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "2"
	       MOVE 6		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "3"
	       MOVE 7		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "4"
	       MOVE 8		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "5"
	       MOVE 9		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "6"
	       MOVE 10		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "7"
	       MOVE 11		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "8"
	       MOVE 12		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "A"
	       MOVE 13		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "B"
	       MOVE 14		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "C"
	       MOVE 15		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "D"
	       MOVE 16		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "E"
	       MOVE 17		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "F"
	       MOVE 18		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "G"
	       MOVE 19		 TO SCREEN-LIN.
	     PERFORM SAVE-ATTR.
	     PERFORM MARK-BLOCK.

       BA18.
	   IF WS-OPTION = "0"
	       GO TO BA999.
	     PERFORM SAVE-SCREEN.
	     MOVE BLOCK-DETAIL	 TO BLOCK-DETAIL1.
	     MOVE SHADOW-DETAIL  TO SHADOW-DETAIL1.
	     MOVE CRT-DETAIL	 TO CRT-DETAIL1.
	   IF WS-OPTION = "1"
	       PERFORM CA000
	   ELSE
	   IF WS-OPTION = "2"
	       PERFORM DA000
	   ELSE
	   IF WS-OPTION = "3"
	       PERFORM EA000
	   ELSE
	   IF WS-OPTION = "4"
	       PERFORM FA000
	   ELSE
	   IF WS-OPTION = "5"
	       PERFORM GA000
	   ELSE
	   IF WS-OPTION = "6"
	       PERFORM HA000
	   ELSE
	   IF WS-OPTION = "7"
	       PERFORM IA000
	   ELSE
	   IF WS-OPTION = "8"
	       PERFORM JA000
	   ELSE
	   IF WS-OPTION = "A"
	       PERFORM KA000
	   ELSE
	   IF WS-OPTION = "B"
	       PERFORM LA000
	   ELSE
	   IF WS-OPTION = "C"
	       PERFORM MA000
	   ELSE
	   IF WS-OPTION = "D"
	       PERFORM NA000
	   ELSE
	   IF WS-OPTION = "E"
	       PERFORM OA000
	   ELSE
	   IF WS-OPTION = "F"
	       PERFORM PA000
	   ELSE
	   IF WS-OPTION = "G"
	       PERFORM QA000
	   ELSE
	   IF WS-OPTION = ">"
	       PERFORM RA000
	   ELSE
	   IF WS-OPTION = "H"
	       PERFORM SA000
	   ELSE
	   IF WS-OPTION = "@"
	       PERFORM TA000.
	     PERFORM RESTORE-SCREEN.
	     MOVE BLOCK-DETAIL1  TO BLOCK-DETAIL.
	     MOVE SHADOW-DETAIL1 TO SHADOW-DETAIL.
	     MOVE CRT-DETAIL1	 TO CRT-DETAIL.
	     GO TO BA05.
       BA999.
	     EXIT.

      /
      *    ****    Q U O T A T I O N S
      *
       CA000	       SECTION 50.
       CA00.
	   IF LS0-LEV < 3
	       MOVE "MNU\NOPMENU"
				 TO PRG-NAME
	       PERFORM AA100
	       DISPLAY MENU-INSTRUCT
	       GO TO CA999.
003240	   IF LS0-SALEV < 1
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO CA999.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 28		 TO SHADE-COL.
	     MOVE 23		 TO SHADE-WIDTH.
	     MOVE 11		 TO SHADE-LINES.
      *	     DISPLAY S04.
	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 0626
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 3 HIGHLIGHT
		     "¿"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
	     DISPLAY "³   " AT 0726
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 3 HIGHLIGHT
		     "SALES QUOTATIONS"
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 7 HIGHLIGHT
		     "    ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
	     DISPLAY "³ " AT 0826
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 3 HIGHLIGHT
		     "1" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " New Quotation       ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
	     DISPLAY "³ " AT 0926
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 3 HIGHLIGHT
		     "2" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Amend Existing Quote³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
	     DISPLAY "³ " AT 1026
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 3 HIGHLIGHT
		     "3" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Delete a Quote      ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
	     DISPLAY "³ " AT 1126
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 3 HIGHLIGHT
		     "4" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Convert to Order    ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
	     DISPLAY "³ " AT 1226
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 3 HIGHLIGHT
		     "5" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Amend Quote Remarks ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
	     DISPLAY "³ " AT 1326
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 3 HIGHLIGHT
		     "6" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Debtor Enquiry      ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
	     DISPLAY "³ " AT 1426
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 3 HIGHLIGHT
		     "7" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Stock Enquiry       ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
	     DISPLAY "³ " AT 1526
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 3 HIGHLIGHT
		     "8" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Picking Slip Format ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
	     DISPLAY "³ " AT 1626
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 3 HIGHLIGHT
		     "0" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Exit                ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
	     DISPLAY "À" AT 1726
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 3 HIGHLIGHT
		     "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.

	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.
	     MOVE 7		 TO TOP-ROW.
	     MOVE 15		 TO BOTTOM-ROW.
	     MOVE 6		 TO SCREEN-LIN.
	     MOVE 28		 TO SCREEN-COL.
	     MOVE 21		 TO STRING-LENGTH.
	     PERFORM SAVE-ATTR.

       CA05.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       CA10.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY	 PERFORM MOVE-BLOCK-UP
		 WHEN DOWN-KEY	 PERFORM MOVE-BLOCK-DOWN
		 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
				 IF MOUSE-ENTER = "Y"
				     MOVE "N"	 TO MOUSE-ENTER
				     GO TO CA15
				 END-IF
				 IF MOUSE-Y = 24
				     IF MOUSE-X = 5
					 MOVE 5  TO KEY-CODE-1
					 GO TO CA10
				     ELSE
				     IF MOUSE-X = 10
					 MOVE 6  TO KEY-CODE-1
					 GO TO CA10
				     ELSE
				     IF MOUSE-X > 36 AND < 44
					 MOVE 0  TO KEY-CODE-1
					 GO TO CA10
				     ELSE
				     IF MOUSE-X > 54 AND < 60
					 MOVE 0  TO KEY-CODE-1
					 MOVE 1  TO KEY-TYPE
					 GO TO CA10
				     END-IF
				 END-IF
		 WHEN ENTER-KEY  GO TO CA15
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CA05
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE "0"	 TO WS-OPTION
		     GO TO CA18
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CA05
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	       IF WS-OPTION < "0" OR > "8"
		   CALL X"E5"
		   GO TO CA05
	       END-IF
	       GO TO CA16
	   ELSE
	       CALL X"E5"
	       GO TO CA05.

       CA15.
	   IF SCREEN-LIN < 7
	       CALL X"E5"
	       GO TO CA05
	   ELSE
	   IF SCREEN-LIN = 7
	       MOVE "1"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 8
	       MOVE "2"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 9
	       MOVE "3"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 10
	       MOVE "4"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 11
	       MOVE "5"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 12
	       MOVE "6"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 13
	       MOVE "7"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 14
	       MOVE "8"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 15
	       MOVE "0"		 TO WS-OPTION.
	   GO TO CA18.

       CA16.
	     PERFORM CLEAR-BLOCK.
	   IF WS-OPTION = "0"
	       MOVE 15		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "1"
	       MOVE 7		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "2"
	       MOVE 8		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "3"
	       MOVE 9		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "4"
	       MOVE 10		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "5"
	       MOVE 11		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "6"
	       MOVE 12		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "7"
	       MOVE 13		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "8"
	       MOVE 14		 TO SCREEN-LIN.
	     PERFORM SAVE-ATTR.
	     PERFORM MARK-BLOCK.

       CA18.
	   IF WS-OPTION = "0"
	       GO TO CA999.
	   IF WS-OPTION = "1"
	       MOVE "SLP\QTP001" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "2"
	       MOVE "SLP\QTP002" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "3"
	       MOVE "SLP\QTP003" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "4"
	       MOVE "SLP\QTP004" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "5"
	       MOVE "SLP\QTP005" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "6"
	       MOVE "DTP\DTPENQ" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "7"
	       MOVE "STP\STPENQ" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "8"
	       MOVE "SLP\PIC001" TO PRG-NAME.
	     PERFORM AA100.
	     DISPLAY MENU-INSTRUCT.
	     GO TO CA05.

       CA999.
	     EXIT.

      /
      *    ****    S A L E S   /   O R D E R S
      *
       DA000	       SECTION 51.
       DA00.
	   IF LS0-LEV < 3
	       MOVE "MNU\NOPMENU"
				 TO PRG-NAME
	       PERFORM AA100
	       DISPLAY MENU-INSTRUCT
	       GO TO DA999.
003240	   IF LS0-SALEV < 1
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO DA999.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 28		 TO SHADE-COL.
	     MOVE 23		 TO SHADE-WIDTH.
	     MOVE 12		 TO SHADE-LINES.
      *	     DISPLAY S06.
	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 0626
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 3 HIGHLIGHT
		     "¿" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
	     DISPLAY "³     " AT 0726
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 3 HIGHLIGHT
		     "SALES ORDERS"
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 7 HIGHLIGHT
		     "      ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
	     DISPLAY "³ " AT 0826
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 3 HIGHLIGHT
		     "1" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Orders              ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
	     DISPLAY "³ " AT 0926
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 3 HIGHLIGHT
		     "2" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Picking Slips       ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
	     DISPLAY "³ " AT 1026
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 3 HIGHLIGHT
		     "3" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Serial Numbers      ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
	     DISPLAY "³ " AT 1126
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 3 HIGHLIGHT
		     "4" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Convert to Invoice  ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
	     DISPLAY "³ " AT 1226
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 3 HIGHLIGHT
		     "5" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Debtor Enquiry      ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
	     DISPLAY "³ " AT 1326
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 3 HIGHLIGHT
		     "6" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Stock Enquiry       ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
	     DISPLAY "³ " AT 1426
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 3 HIGHLIGHT
		     "7" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Outstanding Orders  ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
	     DISPLAY "³ " AT 1526
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 3 HIGHLIGHT
		     "8" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Picking Slip Format ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
	     DISPLAY "³ " AT 1626
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 3 HIGHLIGHT
		     "A" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Order Number System ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
	     DISPLAY "³ " AT 1726
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 3 HIGHLIGHT
		     "0" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Exit	           ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
001570	     DISPLAY "À" AT 1826
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 3 HIGHLIGHT
		     "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.
	     MOVE 7		 TO TOP-ROW.
	     MOVE 16		 TO BOTTOM-ROW.
	     MOVE 6		 TO SCREEN-LIN.
	     MOVE 28		 TO SCREEN-COL.
	     MOVE 21		 TO STRING-LENGTH.
	     PERFORM SAVE-ATTR.

       DA05.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       DA10.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY	 PERFORM MOVE-BLOCK-UP
		 WHEN DOWN-KEY	 PERFORM MOVE-BLOCK-DOWN
		 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
				 IF MOUSE-ENTER = "Y"
				     MOVE "N"	 TO MOUSE-ENTER
				     GO TO DA15
				 END-IF
				 IF MOUSE-Y = 24
				     IF MOUSE-X = 5
					 MOVE 5  TO KEY-CODE-1
					 GO TO DA10
				     ELSE
				     IF MOUSE-X = 10
					 MOVE 6  TO KEY-CODE-1
					 GO TO DA10
				     ELSE
				     IF MOUSE-X > 36 AND < 44
					 MOVE 0  TO KEY-CODE-1
					 GO TO DA10
				     ELSE
				     IF MOUSE-X > 54 AND < 60
					 MOVE 0  TO KEY-CODE-1
					 MOVE 1  TO KEY-TYPE
					 GO TO DA10
				     END-IF
				 END-IF
		 WHEN ENTER-KEY  GO TO DA15
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO DA05
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE "0"	 TO WS-OPTION
		     GO TO DA18
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO DA05
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
	       IF (WS-OPTION < "0" OR > "8") AND
		  (WS-OPTION < "A" OR > "A")
		   CALL X"E5"
		   GO TO DA05
	       END-IF
	       GO TO DA16
	   ELSE
	       CALL X"E5"
	       GO TO DA05.

       DA15.
	   IF SCREEN-LIN < 7
	       CALL X"E5"
	       GO TO DA05
	   ELSE
	   IF SCREEN-LIN = 7
	       MOVE "1"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 8
	       MOVE "2"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 9
	       MOVE "3"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 10
	       MOVE "4"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 11
	       MOVE "5"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 12
	       MOVE "6"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 13
	       MOVE "7"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 14
	       MOVE "8"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 15
	       MOVE "A"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 16
	       MOVE "0"		 TO WS-OPTION.
	   GO TO DA18.

       DA16.
	     PERFORM CLEAR-BLOCK.
	   IF WS-OPTION = "0"
	       MOVE 16		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "1"
	       MOVE 7		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "2"
	       MOVE 8		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "3"
	       MOVE 9		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "4"
	       MOVE 10		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "5"
	       MOVE 11		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "6"
	       MOVE 12		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "7"
	       MOVE 13		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "8"
	       MOVE 14		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "A"
	       MOVE 15		 TO SCREEN-LIN.
	     PERFORM SAVE-ATTR.
	     PERFORM MARK-BLOCK.

       DA18.
	   IF WS-OPTION = "0"
	       GO TO DA999.
	   IF WS-OPTION = "1"
	       MOVE "SLP\SLS001" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "2"
	       MOVE "SLP\SLS002" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "3"
	       MOVE "SLP\SLS004" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "4"
	       MOVE "SLP\SLS003" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "5"
	       MOVE "DTP\DTPENQ" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "6"
	       MOVE "STP\STPENQ" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "7"
	       MOVE "SLP\SLS010" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "8"
	       MOVE "SLP\PIC001" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "A"
	       MOVE "SLP\ORD001" TO PRG-NAME.
	     PERFORM AA100.
	     DISPLAY MENU-INSTRUCT.
	     GO TO DA05.

       DA999.
	     EXIT.

      /
      *    ****    S T A N D I N G   O R D E R S
      *
       EA000	       SECTION 52.
       EA00.
	   IF LS-DSKDRV = "L"
	       MOVE "Not available on Local drive"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO EA999.
	   IF (LS0-LEV < 4) OR (LS0-RTE = 0)
	       MOVE "MNU\NOPMENU"
				 TO PRG-NAME
	       PERFORM AA100
	       DISPLAY MENU-INSTRUCT
	       GO TO EA999.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 28		 TO SHADE-COL.
	     MOVE 23		 TO SHADE-WIDTH.
	     MOVE 9		 TO SHADE-LINES.
      *	     DISPLAY S08.
	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" AT 0626
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
	     DISPLAY "³    " AT 0726
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
		     "STANDING ORDERS"
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 7 HIGHLIGHT
		     "    ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
	     DISPLAY "³ " AT 0826
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
		     "1" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Order Maintenance   ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
	     DISPLAY "³ " AT 0926
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
		     "2" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Route Schedule      ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
	     DISPLAY "³ " AT 1026
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
		     "3" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Delivery Notes      ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
	     DISPLAY "³ " AT 1126
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
		     "4" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Process Deliveries  ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
	     DISPLAY "³ " AT 1226
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
		     "5" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Vehicle Stock       ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
	     DISPLAY "³ " AT 1326
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
		     "6" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Stock Enquiry       ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
	     DISPLAY "³ " AT 1426
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
		     "0" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Exit	           ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
001570	     DISPLAY "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" AT 1526
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.
	     MOVE 7		 TO TOP-ROW.
	     MOVE 13		 TO BOTTOM-ROW.
	     MOVE 6		 TO SCREEN-LIN.
	     MOVE 28		 TO SCREEN-COL.
	     MOVE 21		 TO STRING-LENGTH.
	     PERFORM SAVE-ATTR.

       EA05.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       EA10.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY	 PERFORM MOVE-BLOCK-UP
		 WHEN DOWN-KEY	 PERFORM MOVE-BLOCK-DOWN
		 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
				 IF MOUSE-ENTER = "Y"
				     MOVE "N"	 TO MOUSE-ENTER
				     GO TO EA15
				 END-IF
				 IF MOUSE-Y = 24
				     IF MOUSE-X = 5
					 MOVE 5  TO KEY-CODE-1
					 GO TO EA10
				     ELSE
				     IF MOUSE-X = 10
					 MOVE 6  TO KEY-CODE-1
					 GO TO EA10
				     ELSE
				     IF MOUSE-X > 36 AND < 44
					 MOVE 0  TO KEY-CODE-1
					 GO TO EA10
				     ELSE
				     IF MOUSE-X > 54 AND < 60
					 MOVE 0  TO KEY-CODE-1
					 MOVE 1  TO KEY-TYPE
					 GO TO EA10
				     END-IF
				 END-IF
		 WHEN ENTER-KEY  GO TO EA15
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO EA05
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE "0"	 TO WS-OPTION
		     GO TO EA18
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO EA05
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	       IF WS-OPTION < "0" OR > "6"
		   CALL X"E5"
		   GO TO EA05
	       END-IF
	       GO TO EA16
	   ELSE
	       CALL X"E5"
	       GO TO EA05.

       EA15.
	   IF SCREEN-LIN < 7
	       CALL X"E5"
	       GO TO EA05
	   ELSE
	   IF SCREEN-LIN = 7
	       MOVE "1"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 8
	       MOVE "2"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 9
	       MOVE "3"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 10
	       MOVE "4"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 11
	       MOVE "5"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 12
	       MOVE "6"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 13
	       MOVE "0"		 TO WS-OPTION.
	   GO TO EA18.

       EA16.
	     PERFORM CLEAR-BLOCK.
	   IF WS-OPTION = "0"
	       MOVE 13		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "1"
	       MOVE 7		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "2"
	       MOVE 8		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "3"
	       MOVE 9		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "4"
	       MOVE 10		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "5"
	       MOVE 11		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "6"
	       MOVE 12		 TO SCREEN-LIN.
	     PERFORM SAVE-ATTR.
	     PERFORM MARK-BLOCK.

       EA18.
	   IF WS-OPTION = "0"
	       GO TO EA999.
	   IF WS-OPTION = "1"
	       MOVE "SLP\DLP001" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "2"
	       MOVE "SLP\DLP002" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "3"
	       MOVE "SLP\DLP003" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "4"
	       MOVE "SLP\DLP004" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "5"
	       MOVE "DTP\DTPENQ" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "6"
	       MOVE "STP\STPENQ" TO PRG-NAME.
	     PERFORM AA100.
	     DISPLAY MENU-INSTRUCT.
	     GO TO EA05.

       EA999.
	     EXIT.

      /
      *    ****    D E L I V E R Y   V E H I C L E S
      *
       FA000	       SECTION 53.
       FA00.
	   IF LS-DSKDRV = "L"
	       MOVE "Not available on Local drive"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO FA999.
	   IF (LS0-LEV < 4) OR (LS0-RTE = 0)
	       MOVE "MNU\NOPMENU"
				 TO PRG-NAME
	       PERFORM AA100
	       DISPLAY MENU-INSTRUCT
	       GO TO FA999.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 28		 TO SHADE-COL.
	     MOVE 23		 TO SHADE-WIDTH.
	     MOVE 7		 TO SHADE-LINES.
      *	     DISPLAY S09.
	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" AT 0626
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
	     DISPLAY "³   " AT 0726
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
		     "DELIVERY VEHICLES"
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 7 HIGHLIGHT
		     "   ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
	     DISPLAY "³ " AT 0826
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
		     "1" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Create New Record   ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
	     DISPLAY "³ " AT 0926
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
		     "2" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Amend Existing Rec. ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
	     DISPLAY "³ " AT 1026
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
		     "3" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Delete Existing Rec.³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
	     DISPLAY "³ " AT 1126
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
		     "4" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Enquiries           ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
	     DISPLAY "³ " AT 1226
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
		     "0" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Exit                ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
001570	     DISPLAY "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" AT 1326
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.
	     MOVE 7		 TO TOP-ROW.
	     MOVE 11		 TO BOTTOM-ROW.
	     MOVE 6		 TO SCREEN-LIN.
	     MOVE 28		 TO SCREEN-COL.
	     MOVE 21		 TO STRING-LENGTH.
	     PERFORM SAVE-ATTR.

       FA05.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       FA10.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY	 PERFORM MOVE-BLOCK-UP
		 WHEN DOWN-KEY	 PERFORM MOVE-BLOCK-DOWN
		 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
				 IF MOUSE-ENTER = "Y"
				     MOVE "N"	 TO MOUSE-ENTER
				     GO TO FA15
				 END-IF
				 IF MOUSE-Y = 24
				     IF MOUSE-X = 5
					 MOVE 5  TO KEY-CODE-1
					 GO TO FA10
				     ELSE
				     IF MOUSE-X = 10
					 MOVE 6  TO KEY-CODE-1
					 GO TO FA10
				     ELSE
				     IF MOUSE-X > 36 AND < 44
					 MOVE 0  TO KEY-CODE-1
					 GO TO FA10
				     ELSE
				     IF MOUSE-X > 54 AND < 60
					 MOVE 0  TO KEY-CODE-1
					 MOVE 1  TO KEY-TYPE
					 GO TO FA10
				     END-IF
				 END-IF
		 WHEN ENTER-KEY  GO TO FA15
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO FA05
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE "0"	 TO WS-OPTION
		     GO TO FA18
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO FA05
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	       IF WS-OPTION < "0" OR > "4"
		   CALL X"E5"
		   GO TO FA05
	       END-IF
	       GO TO FA16
	   ELSE
	       CALL X"E5"
	       GO TO FA05.

       FA15.
	   IF SCREEN-LIN < 7
	       CALL X"E5"
	       GO TO FA05
	   ELSE
	   IF SCREEN-LIN = 7
	       MOVE "1"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 8
	       MOVE "2"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 9
	       MOVE "3"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 10
	       MOVE "4"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 11
	       MOVE "0"		 TO WS-OPTION.
	   GO TO FA18.

       FA16.
	     PERFORM CLEAR-BLOCK.
	   IF WS-OPTION = "0"
	       MOVE 11		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "1"
	       MOVE 7		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "2"
	       MOVE 8		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "3"
	       MOVE 9		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "4"
	       MOVE 10		 TO SCREEN-LIN.
	     PERFORM SAVE-ATTR.
	     PERFORM MARK-BLOCK.

       FA18.
	   IF WS-OPTION = "0"
	       GO TO FA999.
	   IF WS-OPTION = "1"
	       MOVE "SLP\DLP021" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "2"
	       MOVE "SLP\DLP022" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "3"
	       MOVE "SLP\DLP023" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "4"
	       MOVE "SLP\DLP024" TO PRG-NAME.
	     PERFORM AA100.
	     DISPLAY MENU-INSTRUCT.
	     GO TO FA05.

       FA999.
	     EXIT.

      /
      *    ****    P R O C E S S   D E L I V E R I E S
      *
       GA000	       SECTION 54.
       GA00.
	   IF LS-DSKDRV = "L"
	       MOVE "Not available on Local drive"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO GA999.
	   IF (LS0-LEV < 4) OR (LS0-RTE = 0)
	       MOVE "MNU\NOPMENU"
				 TO PRG-NAME
	   ELSE
	       MOVE "SLP\DLP003" TO PRG-NAME.
003240	   IF LS0-SALEV < 3
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO GA999.
	     PERFORM AA100.

       GA999.
	     EXIT.

      /
      *    ****    I N V O I C I N G   /   C R E D I T	 N O T E S
      *
       HA000	       SECTION 55.
       HA00.
	   IF LS-DSKDRV = "L"
	       MOVE "Not available on Local drive"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO HA999.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 28		 TO SHADE-COL.
	     MOVE 24		 TO SHADE-WIDTH.
	     MOVE 16		 TO SHADE-LINES.
      *	     DISPLAY S03.
	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 0626
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 3 HIGHLIGHT
		     "¿" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
	     DISPLAY "³ " AT 0726
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 3 HIGHLIGHT
		     "INVOICES/CREDIT NOTES"
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 7 HIGHLIGHT
		     "  ³" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
	     DISPLAY "³ " AT 0826
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 3 HIGHLIGHT
		     "1" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Invoices (Stock)     ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
001470	     DISPLAY "³ " AT 0926
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 3 HIGHLIGHT
		     "2" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Cash Sale (Stock)    ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
001570	     DISPLAY "³ " AT 1026
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 3 HIGHLIGHT
		     "3" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Credit Note (Stock)  ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
001570	     DISPLAY "³ " AT 1126
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 3 HIGHLIGHT
		     "4" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Cash Refund (Stock)  ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
001570	     DISPLAY "³ " AT 1226
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 3 HIGHLIGHT
		     "5" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Open Cash Drawer     ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
001570	     DISPLAY "³ " AT 1326
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 3 HIGHLIGHT
		     "6" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Balancing Report     ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
001570	     DISPLAY "³ " AT 1426
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 3 HIGHLIGHT
		     "7" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Adjust Float         ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
001570	     DISPLAY "³ " AT 1526
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 3 HIGHLIGHT
		     "8" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Day/Month/Year-End   ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
001570	     DISPLAY "³ " AT 1626
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 3 HIGHLIGHT
		     "A" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Invoice Heading      ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
001570	     DISPLAY "³ " AT 1726
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 3 HIGHLIGHT
		     "B" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Lookup/Re-Print Inv  ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
001570	     DISPLAY "³ " AT 1826
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 3 HIGHLIGHT
		     "C" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Invoices (Non Stock) ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
001570	     DISPLAY "³ " AT 1926
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 3 HIGHLIGHT
		     "D" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Cash Sale Statistics ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
001570	     DISPLAY "³ " AT 2026
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 3 HIGHLIGHT
		     "E" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT.
	   IF LS0-LEV < 3
	       DISPLAY " Amend Inv. Remrks N/A³" AT 2029
			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
	   ELSE
	       DISPLAY " Amend Inv. Remarks   ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
001570	     DISPLAY "³ " AT 2126
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 3 HIGHLIGHT
		     "0" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Exit	            ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
	     DISPLAY "À" AT 2226
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 3 HIGHLIGHT
		     "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.
	     MOVE 7		 TO TOP-ROW.
	     MOVE 20		 TO BOTTOM-ROW.
	     MOVE 6		 TO SCREEN-LIN.
	     MOVE 28		 TO SCREEN-COL.
	     MOVE 22		 TO STRING-LENGTH.
	     PERFORM SAVE-ATTR.

       HA05.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       HA10.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY	 PERFORM MOVE-BLOCK-UP
		 WHEN DOWN-KEY	 PERFORM MOVE-BLOCK-DOWN
		 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
				 IF MOUSE-ENTER = "Y"
				     MOVE "N"	 TO MOUSE-ENTER
				     GO TO HA15
				 END-IF
				 IF MOUSE-Y = 24
				     IF MOUSE-X = 5
					 MOVE 5  TO KEY-CODE-1
					 GO TO HA10
				     ELSE
				     IF MOUSE-X = 10
					 MOVE 6  TO KEY-CODE-1
					 GO TO HA10
				     ELSE
				     IF MOUSE-X > 36 AND < 44
					 MOVE 0  TO KEY-CODE-1
					 GO TO HA10
				     ELSE
				     IF MOUSE-X > 54 AND < 60
					 MOVE 0  TO KEY-CODE-1
					 MOVE 1  TO KEY-TYPE
					 GO TO HA10
				     END-IF
				 END-IF
		 WHEN ENTER-KEY  GO TO HA15
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO HA05
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE "0"	 TO WS-OPTION
		     GO TO HA18
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO HA05
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
	       IF (WS-OPTION < "0" OR > "8") AND
		  (WS-OPTION < "A" OR > "E")
		   CALL X"E5"
		   GO TO HA05
	       END-IF
	       GO TO HA16
	   ELSE
	       CALL X"E5"
	       GO TO HA05.

       HA15.
	   IF SCREEN-LIN < 7
	       CALL X"E5"
	       GO TO HA05
	   ELSE
	   IF SCREEN-LIN = 7
	       MOVE "1"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 8
	       MOVE "2"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 9
	       MOVE "3"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 10
	       MOVE "4"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 11
	       MOVE "5"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 12
	       MOVE "6"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 13
	       MOVE "7"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 14
	       MOVE "8"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 15
	       MOVE "A"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 16
	       MOVE "B"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 17
	       MOVE "C"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 18
	       MOVE "D"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 19
	       MOVE "E"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 20
	       MOVE "0"		 TO WS-OPTION.
	   GO TO HA18.

       HA16.
	     PERFORM CLEAR-BLOCK.
	   IF WS-OPTION = "0"
	       MOVE 20		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "1"
	       MOVE 7		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "2"
	       MOVE 8		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "3"
	       MOVE 9		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "4"
	       MOVE 10		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "5"
	       MOVE 11		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "6"
	       MOVE 12		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "7"
	       MOVE 13		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "8"
	       MOVE 14		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "A"
	       MOVE 15		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "B"
	       MOVE 16		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "C"
	       MOVE 17		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "D"
	       MOVE 18		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "E"
	       MOVE 19		 TO SCREEN-LIN.
	     PERFORM SAVE-ATTR.
	     PERFORM MARK-BLOCK.

       HA18.
	   IF WS-OPTION = "0"
	       GO TO HA999.
	   IF WS-OPTION = "1"
	       MOVE "INV\DTPINV" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "2"
	       MOVE "INV\DTPCSH" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "3"
	       MOVE "INV\DTPCRN" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "4"
	       MOVE "INV\DTPREF" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "5"
	       MOVE "POS\OPNDRW" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "6"
	       MOVE "POS\CSHBAL" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "7"
	       MOVE "POS\CSHFLT" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "8"
	       MOVE "POS\CSHEND" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "A"
	       MOVE "INV\INVHDG" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "B"
	       MOVE "INV\INVDUP" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "C"
	       MOVE "INV\DTPIVP" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "D"
	       MOVE "POS\CSTATS" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "E"
	       IF LS0-LEV < 3
		   MOVE "MNU\NOPMENU" TO PRG-NAME
	       ELSE
		   MOVE "INV\INVRMK" TO PRG-NAME.
	     PERFORM AA100.
	     DISPLAY MENU-INSTRUCT.
	     GO TO HA05.
       HA999.
	     EXIT.
      /
      *    ****    C A S H   S A L E S	 ( P O S )
      *
       IA000	       SECTION 56.
       IA00.
	   IF LS0-POS = 0
	       MOVE "MNU\NOPMENU"
				 TO PRG-NAME
	       PERFORM AA100
	       DISPLAY MENU-INSTRUCT
	       GO TO IA999.
003240	   IF LS0-SALEV < 1
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO IA999.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 28		 TO SHADE-COL.
	     MOVE 24		 TO SHADE-WIDTH.
	     MOVE 13		 TO SHADE-LINES.
      *	     DISPLAY S05.
	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 0626
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 3 HIGHLIGHT
		     "¿" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
	     DISPLAY "³    " AT 0726
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 3 HIGHLIGHT
		     "CASH SALES (POS)"
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 7 HIGHLIGHT
		     "    ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
	     DISPLAY "³ " AT 0826
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 3 HIGHLIGHT
		     "1" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Cash Sale (Stock)    ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
001470	     DISPLAY "³ " AT 0926
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 3 HIGHLIGHT
		     "2" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Open Cash Drawer     ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
001570	     DISPLAY "³ " AT 1026
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 3 HIGHLIGHT
		     "3" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Balancing Report     ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
001570	     DISPLAY "³ " AT 1126
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 3 HIGHLIGHT
		     "4" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Adjust Float         ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
001570	     DISPLAY "³ " AT 1226
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 3 HIGHLIGHT
		     "5" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Day/Month/Year-End   ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
001570	     DISPLAY "³ " AT 1326
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 3 HIGHLIGHT
		     "6" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Cash Refund (Stock)  ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
001570	     DISPLAY "³ " AT 1426
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 3 HIGHLIGHT
		     "7" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Initialize Drawer    ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
001570	     DISPLAY "³ " AT 1526
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 3 HIGHLIGHT
		     "8" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Cash Sale Statistics ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
001570	     DISPLAY "³ " AT 1626
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 3 HIGHLIGHT
		     "A" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Petty Cash Voucher   ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
001570	     DISPLAY "³ " AT 1726
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 3 HIGHLIGHT
		     "B" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Picking Slips	    ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
001570	     DISPLAY "³ " AT 1826
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 3 HIGHLIGHT
		     "0" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Exit	            ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
	     DISPLAY "À" AT 1926
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 3 HIGHLIGHT
		     "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.
	     MOVE 7		 TO TOP-ROW.
	     MOVE 17		 TO BOTTOM-ROW.
	     MOVE 6		 TO SCREEN-LIN.
	     MOVE 28		 TO SCREEN-COL.
	     MOVE 22		 TO STRING-LENGTH.
	     PERFORM SAVE-ATTR.

       IA05.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       IA10.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY	 PERFORM MOVE-BLOCK-UP
		 WHEN DOWN-KEY	 PERFORM MOVE-BLOCK-DOWN
		 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
				 IF MOUSE-ENTER = "Y"
				     MOVE "N"	 TO MOUSE-ENTER
				     GO TO IA15
				 END-IF
				 IF MOUSE-Y = 24
				     IF MOUSE-X = 5
					 MOVE 5  TO KEY-CODE-1
					 GO TO IA10
				     ELSE
				     IF MOUSE-X = 10
					 MOVE 6  TO KEY-CODE-1
					 GO TO IA10
				     ELSE
				     IF MOUSE-X > 36 AND < 44
					 MOVE 0  TO KEY-CODE-1
					 GO TO IA10
				     ELSE
				     IF MOUSE-X > 54 AND < 60
					 MOVE 0  TO KEY-CODE-1
					 MOVE 1  TO KEY-TYPE
					 GO TO IA10
				     END-IF
				 END-IF
		 WHEN ENTER-KEY  GO TO IA15
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO IA05
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE "0"	 TO WS-OPTION
		     GO TO IA18
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO IA05
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
	       IF (WS-OPTION < "0" OR > "8") AND
		  (WS-OPTION < "A" OR > "B")
		   CALL X"E5"
		   GO TO IA05
	       END-IF
	       GO TO IA16
	   ELSE
	       CALL X"E5"
	       GO TO IA05.

       IA15.
	   IF SCREEN-LIN < 7
	       CALL X"E5"
	       GO TO IA05
	   ELSE
	   IF SCREEN-LIN = 7
	       MOVE "1"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 8
	       MOVE "2"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 9
	       MOVE "3"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 10
	       MOVE "4"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 11
	       MOVE "5"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 12
	       MOVE "6"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 13
	       MOVE "7"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 14
	       MOVE "8"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 15
	       MOVE "A"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 16
	       MOVE "B"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 17
	       MOVE "0"		 TO WS-OPTION.
	   GO TO IA18.

       IA16.
	     PERFORM CLEAR-BLOCK.
	   IF WS-OPTION = "0"
	       MOVE 17		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "1"
	       MOVE 7		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "2"
	       MOVE 8		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "3"
	       MOVE 9		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "4"
	       MOVE 10		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "5"
	       MOVE 11		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "6"
	       MOVE 12		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "7"
	       MOVE 13		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "8"
	       MOVE 14		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "A"
	       MOVE 15		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "B"
	       MOVE 16		 TO SCREEN-LIN.
	     PERFORM SAVE-ATTR.
	     PERFORM MARK-BLOCK.

       IA18.
	   IF WS-OPTION = "0"
	       GO TO IA999.
	   IF WS-OPTION = "1"
	       IF LS0-NO = 003
		   IF WS-SYS-ID = "MDE" OR "mde"
		       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260		       PERFORM ERROR-MESSAGE
		       GO TO IA05.
	   IF WS-OPTION = "1"
	       MOVE "POS\POS001" TO PRG-NAME
	       PERFORM AA100
	       IF LS-INSTR = "E"
		   MOVE SPACE	 TO LS-INSTR
		   GO TO IA20
	       ELSE
	       IF LS-INSTR = "A"
		   IF LS0-POS > 0
		       MOVE "INV\DTPPOS"
				 TO PRG-NAME
		   ELSE
		       MOVE "INV\DTPINV"
				 TO PRG-NAME
		   END-IF
		   PERFORM AA100
		   MOVE "1"	 TO WS-OPTION
		   GO TO IA18
	       ELSE
	       IF LS-INSTR = "R"
		   MOVE "POS\CSH002"
				 TO PRG-NAME
		   PERFORM AA100
		   MOVE "1"	 TO WS-OPTION
		   GO TO IA18
	       ELSE
	       IF LS-INSTR = "P"
		   MOVE "POS\CSH003"
				 TO PRG-NAME
		   PERFORM AA100
		   MOVE "1"	 TO WS-OPTION
		   GO TO IA18
	       END-IF
	   ELSE
	   IF WS-OPTION = "2"
	       MOVE "POS\OPNDRW" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "3"
	       MOVE "POS\CSHBAL" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "4"
	       MOVE "POS\CSHFLT" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "5"
	       MOVE "POS\CSHEND" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "6"
	       MOVE "POS\CSH002" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "7"
	       MOVE "POS\CSHINT" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "8"
	       MOVE "POS\CSTATS" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "A"
	       MOVE "POS\CSH003" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "B"
	       MOVE "S" 	 TO LS-INSTR
	       MOVE "POS\POS001" TO PRG-NAME.
	     PERFORM AA100.
	     MOVE SPACE		 TO LS-INSTR.

       IA20.
	     DISPLAY MENU-INSTRUCT.
	     GO TO IA05.

       IA999.
	     EXIT.

      /
      *    ****    D E B T O R	 R E C E I P T S
      *
       JA000	       SECTION 57.
       JA00.
	   IF LS-DSKDRV = "L"
	       MOVE "Not available on Local drive"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO JA999.
003240	   IF LS0-DBLEV < 1
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO JA999.
	     MOVE "DTP\DTP012"	 TO PRG-NAME.
	     PERFORM AA100.

       JA999.
	     EXIT.

      /
      *    ****    G U A R A N T E E   M O D U L E
      *
       KA000	       SECTION 58.
       KA00.
	   IF LS-DSKDRV = "L"
	       MOVE "Not available on Local drive"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO KA999.
	   IF LS0-LEV < 3
	       MOVE "MNU\NOPMENU"
				 TO PRG-NAME
	   ELSE
	       MOVE "STP\GAR001" TO PRG-NAME.
003240	   IF (LS0-STLEV < 1) OR (LS0-SALEV < 1)
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO KA999.
	     PERFORM AA100.

       KA999.
	     EXIT.
      /
      *    ****    E N Q U I R I E S
      *
       LA000	       SECTION 59.
       LA00.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 28		 TO SHADE-COL.
	     MOVE 18		 TO SHADE-WIDTH.
	     MOVE 5		 TO SHADE-LINES.
      *	     DISPLAY S07.
	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" AT 0626
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
	     DISPLAY "³    " AT 0726
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
		     "ENQUIRIES" WITH BACKGROUND-COLOR 3
				      FOREGROUND-COLOR 7 HIGHLIGHT
		     "     ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
	     DISPLAY "³ " AT 0826
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
		     "1" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Debtor Account ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
001470	     DISPLAY "³ " AT 0926
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
		     "2" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Stock Record   ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
001570	     DISPLAY "³ " AT 1026
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
		     "0" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Exit	      ³"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
001500	     DISPLAY "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" AT 1126
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.
	     MOVE 7		 TO TOP-ROW.
	     MOVE 9		 TO BOTTOM-ROW.
	     MOVE 6		 TO SCREEN-LIN.
	     MOVE 28		 TO SCREEN-COL.
	     MOVE 16		 TO STRING-LENGTH.
	     PERFORM SAVE-ATTR.

       LA05.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       LA10.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY	 PERFORM MOVE-BLOCK-UP
		 WHEN DOWN-KEY	 PERFORM MOVE-BLOCK-DOWN
		 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
				 IF MOUSE-ENTER = "Y"
				     MOVE "N"	 TO MOUSE-ENTER
				     GO TO LA15
				 END-IF
				 IF MOUSE-Y = 24
				     IF MOUSE-X = 5
					 MOVE 5  TO KEY-CODE-1
					 GO TO LA10
				     ELSE
				     IF MOUSE-X = 10
					 MOVE 6  TO KEY-CODE-1
					 GO TO LA10
				     ELSE
				     IF MOUSE-X > 36 AND < 44
					 MOVE 0  TO KEY-CODE-1
					 GO TO LA10
				     ELSE
				     IF MOUSE-X > 54 AND < 60
					 MOVE 0  TO KEY-CODE-1
					 MOVE 1  TO KEY-TYPE
					 GO TO LA10
				     END-IF
				 END-IF
		 WHEN ENTER-KEY  GO TO LA15
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO LA05
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE "0"	 TO WS-OPTION
		     GO TO LA18
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO LA05
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	       IF WS-OPTION < 0 OR > 2
		   CALL X"E5"
		   GO TO LA05
	       END-IF
	       GO TO LA16
	   ELSE
	       CALL X"E5"
	       GO TO LA05.

       LA15.
	   IF SCREEN-LIN < 7
	       CALL X"E5"
	       GO TO LA05
	   ELSE
	   IF SCREEN-LIN = 7
	       MOVE "1"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 8
	       MOVE "2"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 9
	       MOVE "0"		 TO WS-OPTION.
	   GO TO LA18.

       LA16.
	     PERFORM CLEAR-BLOCK.
	   IF WS-OPTION = 0
	       MOVE 9		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = 1
	       MOVE 7		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = 2
	       MOVE 8		 TO SCREEN-LIN.
	     PERFORM SAVE-ATTR.
	     PERFORM MARK-BLOCK.

       LA18.
	   IF WS-OPTION = "0"
	       GO TO LA999.
	   IF WS-OPTION = "1"
	       MOVE "DTP\DTPENQ" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "2"
	       MOVE "STP\STPENQ" TO PRG-NAME.
	     PERFORM AA100.
	     DISPLAY MENU-INSTRUCT.
	     GO TO LA05.

       LA999.
	     EXIT.

      /
      *    ****    S T O C K   /   O R D E R S
      *
       MA000	       SECTION 60.
       MA00.
	   IF LS-DSKDRV = "L"
	       MOVE "Not available on Local drive"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO MA999.
	   IF LS0-LEV < 3
	       MOVE "MNU\NOPMENU"
				 TO PRG-NAME
	       GO TO MA05.
003240	   IF (LS0-STLEV < 1) AND (LS0-SALEV < 1)
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO MA999.
	     MOVE "STP\STPSOR"	 TO PRG-NAME.
	     PERFORM AA100.
	     MOVE "STP\ORDPRN2"  TO PRG-NAME.

       MA05.
	     PERFORM AA100.

       MA999.
	     EXIT.

      /
      *    ****    S A L E S   S T A T I S T I C S
      *
       NA000	       SECTION 61.
       NA00.
	   IF LS-DSKDRV = "L"
	       MOVE "Not available on Local drive"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO NA999.
	   IF LS0-LEV < 2
	       MOVE "MNU\NOPMENU"
				 TO PRG-NAME
	   ELSE
	       MOVE "STP\STPTOP" TO PRG-NAME.
003240	   IF (LS0-SALEV < 3) AND (LS0-STLEV < 3)
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO NA999.
	     PERFORM AA100.

       NA999.
	     EXIT.

      /
      *    ****    S A L E S   S T A F F   S T A T I S T I C S
      *
       OA000	       SECTION 61.
       OA00.
	   IF LS-DSKDRV = "L"
	       MOVE "Not available on Local drive"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO OA999.
	   IF LS0-LEV < 3
	       MOVE "MNU\NOPMENU"
				 TO PRG-NAME
	   ELSE
	       MOVE "UTP\SALMAN" TO PRG-NAME.
003240	   IF LS0-PALEV < 4
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO OA999.
	     PERFORM AA100.

       OA999.
	     EXIT.
      /
      *    ****    S A L E S   P E R   L E D G E R
      *
       PA000	       SECTION 61.
       PA00.
	   IF LS-DSKDRV = "L"
	       MOVE "Not available on Local drive"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO PA999.
	   IF LS0-LEV < 2
	       MOVE "MNU\NOPMENU"
				 TO PRG-NAME
	   ELSE
	       MOVE "UTP\STPSALES"
				 TO PRG-NAME.
003240	   IF LS0-STLEV < 3
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO PA999.
	     PERFORM AA100.

       PA999.
	     EXIT.

      /
      *    ****    B A R   C O D E   L A B E L S
      *
       QA000	       SECTION 61.
       QA00.
	   IF LS0-LEV < 3
	       MOVE "MNU\NOPMENU"
				 TO PRG-NAME
	   ELSE
	       MOVE "UTP\STPBAR" TO PRG-NAME.
003240	   IF LS0-STLEV < 2
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO QA999.
	     PERFORM AA100.

       QA999.
	     EXIT.
      /
      *    ****    F I X   P I C K I N G   S L I P   Q U A N T I T I E S
      *
       RA000	       SECTION 62.
       RA00.
	     MOVE "STP\STPPIC"	 TO PRG-NAME.
	     PERFORM AA100.

       RA999.
	     EXIT.
      /
      *    ****    D E B T O R	 R E C E I P T S
      *
       SA000	       SECTION 63.
       SA00.
	   IF LS-DSKDRV = "L"
	       MOVE "Not available on Local drive"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO SA999.
003240	   IF LS0-DBLEV < 1
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO SA999.
	     MOVE "DTP\DTP012R"	 TO PRG-NAME.
	     PERFORM AA100.

       SA999.
	     EXIT.

      /
      *    ****    D E L E T E	 N O - S A L E	 I N V O I C E
      * 	   H I S T O R Y
      *
       TA000	       SECTION 64.
       TA00.
	   IF LS-DSKDRV = "L"
	       MOVE "Not available on Local drive"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO SA999.
003240	   IF LS0-DBLEV < 1
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO SA999.
	     MOVE "INV\INVDEL"	 TO PRG-NAME.
	     PERFORM AA100.

       TA999.
	     EXIT.

      /    *************************************************************
      * 		I N I T I A L I S E   P R O G R A M
      *    *************************************************************
038140 ZA000-INIT	       SECTION 90.
038150 ZA000-OPEN.
038160	     PERFORM ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	   S e t   u p	 t h e	 F U N C T I O N   k e y s     ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE 1		 TO USER-ACTION
				    USER-SETTING.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		       ESC, F1 to F10 keys		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE ZERO		 TO USER-NUMBER.
	     MOVE 11		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		    PAGE-UP and PAGE-DOWN keys		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE 53		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		   A C T I V A T E   M O U S E		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE 64		 TO MOUSE-FUNC.
	     MOVE 1		 TO MOUSE-PARAM.
	     CALL X"AF" USING MOUSE-FUNC
			      MOUSE-PARAM.
	   IF MOUSE-FUNC NOT = 255
	       MOVE "Y"		 TO MOUSE-ATTACHED
	   ELSE
	       GO TO ZA00A.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	       Set MOUSE key to act as FUNCTION key	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE 3		 TO USER-ACTION.
	     MOVE 27		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	    Setup the file ID's including path names           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      *    *************************************************************
      *    **** 	  P A R A M E T E R   F I L E		    ****
      *    *************************************************************
       ZA00A.
	     MOVE "PARAM"	 TO AFID-KEY.

       ZA00-READ-APACFIDS.
000030	     READ APACFIDS WITH IGNORE LOCK
	       KEY IS AFID-KEY.
	   IF WS-STATUS = "00"
	       GO TO ZA00-READ-APACFIDS-EXIT.
006260	     STRING "Missing " DELIMITED SIZE
		     AFID-KEY DELIMITED BY " "
		     " file ID - Status " DELIMITED SIZE
		     WS-STATUS DELIMITED SIZE
		     INTO WS-ERR-MES.
	     PERFORM ERROR-LENGTH THRU ERROR-EXIT.
006370	     STOP RUN.

       ZA00-READ-APACFIDS-EXIT.
	     EXIT.

       ZA00A-CONTINUE.
	     MOVE AFID-PATH	 TO W02-PARAM.
      *    *************************************************************
      *    **** 	D A I L Y   -	A U D I T   F I L E	    ****
      *    *************************************************************
	     MOVE "AUDITF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-AUDITF.
      *    *************************************************************
      *    ****       S A L E S	  -   C A R D E X   F I L E	    ****
      *    *************************************************************
	     MOVE "CARDEX"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CARDEX.
      *    *************************************************************
      *    *** C R E D I T O R	 A C C O U N T	 ( A / P )   F I L E ***
      *    *************************************************************
	     MOVE "CREDIT"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CREDIT.
      *    *************************************************************
      *    ****    D E B T O R	 T R A N S A C T I O N	 F I L E    ****
      *    *************************************************************
	     MOVE "DBTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DBTRAN.
      *    *************************************************************
      *    *** D E B T O R   A L T E R N A T E	 I N D E X   F I L E ***
      *    *************************************************************
	     MOVE "DEBALT"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBALT.
      *    *************************************************************
      *    ****     ( A / R )	D E B T O R   M E M O	F I L E	    ****
      *    *************************************************************
	     MOVE "DEBMEM"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBMEM.
      *    *************************************************************
      *    ****  D E B T O R   A C C O U N T   ( A / R )   F I L E  ****
      *    *************************************************************
	     MOVE "DEBTOR"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBTOR.
      *    *************************************************************
      *    **** 	    G U A R A N T E E	F I L E		    ****
      *    *************************************************************
	     MOVE "GARTEE"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-GARTEE.
      *    *************************************************************
      *    ****  ( A / R )   D E B T O R   I N V O I C E  F I L E   ****
      *    *************************************************************
	     MOVE "INVOIC"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-INVOIC.
      *    *************************************************************
      *    **** 	  I N T E G R A T I O N	  F I L E	    ****
      *    *************************************************************
	     MOVE "LEDTRF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-LEDTRF.
      *    *************************************************************
      *    R O U T E   F I L E	 -   C U R R E N T   D E V E L O P M E N
      *    *************************************************************
	     MOVE "ROUTE"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-ROUTE.
      *    *************************************************************
      *    **** 	   S A L E S   O R D E R   F I L E	    ****
      *    *************************************************************
	     MOVE "SORDER"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SORDER.
      *	   *************************************************************
      *    *** S T O C K   ( I V E N T O R Y )	 P A R T S   F I L E ***
      *    *************************************************************
	     MOVE "SPARTS"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SPARTS.
      *    *************************************************************
      *    ****	S T O C K   A L T E R N A T E	I N D E X   F I L E ****
      *    *************************************************************
	     MOVE "STKALT"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STKALT.
      *    *************************************************************
      *    ***	S T O C K   ( I V E N T O R Y )   M E M O   F I L E  ***
      *    *************************************************************
	     MOVE "STKMEM"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STKMEM.
      *    *************************************************************
      *    ****      S T O C K	 ( I V E N T O R Y )   F I L E	    ****
      *    *************************************************************
	     MOVE "STOCKF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STOCKF.
      *    *************************************************************
      *    **** 	    W A R E H O U S E	F I L E		    ****
      *    *************************************************************
	     MOVE "WARHSE"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-WARHSE.
      *    *************************************************************
      *    W A R E H O U S E  -  S T O C K  ( I V E N T O R Y )  F I L E
      *    *************************************************************
	     MOVE "WSTOCK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-WSTOCK.
053630	     OPEN I-O AUDIT.
053640     IF RUNTIME-ERROR
053650         IF FLE-LOCKED
053660             GO TO ZA200
053670         ELSE
053680         IF FLE-LIMIT
053690             GO TO ZA49.
053700     IF WS-STATUS NOT = "00"
	       IF NOT (WS-STATUS = "41")
053710		   MOVE 1	 TO WS-F-ERROR
053720		   PERFORM OPEN-ERROR.
042240	     OPEN INPUT CREDIT.
042250     IF RUNTIME-ERROR
               IF FLE-LOCKED
042130             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
	       IF IDX-CORRUPT
                   MOVE 3        TO WS-F-ERROR
                   GO TO ZA50.
050190     IF WS-STATUS NOT = "00"
050200         MOVE 3            TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
050210		   PERFORM OPEN-ERROR.
053830	     OPEN I-O CARDEX.
	   IF WS-STATUS = "05"
	       CLOSE CARDEX
	       OPEN OUTPUT CARDEX
	       CLOSE CARDEX
	       OPEN I-O CARDEX.
053840	   IF RUNTIME-ERROR
053850         IF FLE-LOCKED
053860             GO TO ZA200
053870         ELSE
053880         IF FLE-LIMIT
053890		   GO TO ZA49
               ELSE
	       IF IDX-CORRUPT
		   MOVE 43	 TO WS-F-ERROR
		   GO TO ZA50.
053900     IF WS-STATUS NOT = "00"
053910	       MOVE 43		 TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
053920		   PERFORM OPEN-ERROR.
053830	     OPEN I-O DBTRAN.
053840     IF RUNTIME-ERROR
053850         IF FLE-LOCKED
053860             GO TO ZA200
053870         ELSE
053880         IF FLE-LIMIT
053890		   GO TO ZA49
               ELSE
	       IF IDX-CORRUPT
                   MOVE 5        TO WS-F-ERROR
		   GO TO ZA50.
053900     IF WS-STATUS NOT = "00"
053910         MOVE 5            TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
053920		   PERFORM OPEN-ERROR.
042210	     OPEN I-O DEBALT.
	   IF WS-STATUS = "05"
	       CLOSE DEBALT
	       OPEN OUTPUT DEBALT
	       CLOSE DEBALT
	       OPEN I-O DEBALT
	   END-IF.
042220     IF RUNTIME-ERROR
               IF FLE-LOCKED
042230             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
	       IF IDX-CORRUPT
		   MOVE 47	 TO WS-F-ERROR
		   GO TO ZA50
	       END-IF
	   END-IF.
042240     IF WS-STATUS NOT = "00"
042250	       MOVE 47		 TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
      *
      *    ****    F I L E   I S   O P E N
      *
	       IF NOT (WS-STATUS = "41")
050210		   PERFORM OPEN-ERROR
	       END-IF
	   END-IF.
053930	     OPEN I-O DEBTOR.
053940     IF RUNTIME-ERROR
053950         IF FLE-LOCKED
053960             GO TO ZA200
053970         ELSE
053980         IF FLE-LIMIT
053990             GO TO ZA49
054200         ELSE
054210	       IF IDX-CORRUPT
054220             MOVE 6        TO WS-F-ERROR
054230             GO TO ZA50.
054000     IF WS-STATUS NOT = "00"
054010         MOVE 6            TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
054020		   PERFORM OPEN-ERROR.
             OPEN I-O DEBMEM.
044310     IF RUNTIME-ERROR
               IF FLE-LOCKED
040420             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
	       IF IDX-CORRUPT
                   MOVE 27       TO WS-F-ERROR
                   GO TO ZA50.
	    IF WS-STATUS NOT = "00"
		MOVE 27 	 TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
		   PERFORM OPEN-ERROR.
054030	     OPEN I-O GARTEE.
054040     IF RUNTIME-ERROR
054050         IF FLE-LOCKED
054060             GO TO ZA200
054070         ELSE
054080         IF FLE-LIMIT
054090             GO TO ZA49
054200         ELSE
054210	       IF IDX-CORRUPT
054220             MOVE 9        TO WS-F-ERROR
054230             GO TO ZA50.
054100     IF WS-STATUS NOT = "00"
054110         MOVE 9            TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
054120		   PERFORM OPEN-ERROR.
054130       OPEN I-O INVOICE.
054140     IF RUNTIME-ERROR
054150         IF FLE-LOCKED
054160             GO TO ZA200
054170         ELSE
054180         IF FLE-LIMIT
054190             GO TO ZA49
054200         ELSE
054210	       IF IDX-CORRUPT
054220             MOVE 12       TO WS-F-ERROR
054230             GO TO ZA50.
054240     IF WS-STATUS NOT = "00"
054250         MOVE 12           TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
054260		   PERFORM OPEN-ERROR.
053830	     OPEN I-O ROUTE.
	   IF WS-STATUS = "05"
	       CLOSE ROUTE
	       OPEN OUTPUT ROUTE
	       CLOSE ROUTE
	       OPEN I-O ROUTE.
053840	   IF RUNTIME-ERROR
053850         IF FLE-LOCKED
053860             GO TO ZA200
053870         ELSE
053880         IF FLE-LIMIT
053890		   GO TO ZA49
               ELSE
	       IF IDX-CORRUPT
		   MOVE 30	 TO WS-F-ERROR
		   GO TO ZA50.
053900     IF WS-STATUS NOT = "00"
053910	       MOVE 30		 TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
053920		   PERFORM OPEN-ERROR.
038970	     OPEN I-O SORDER.
038980     IF RUNTIME-ERROR
038990         IF FLE-LOCKED
039000             GO TO ZA200
039010         ELSE
039020         IF FLE-LIMIT
039030             GO TO ZA49
039040         ELSE
039050	       IF IDX-CORRUPT
039060             MOVE 20       TO WS-F-ERROR
039070             GO TO ZA50.
039080     IF WS-STATUS NOT = "00"
039090         MOVE 20           TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
039100		   PERFORM OPEN-ERROR.
044300	     OPEN I-O LEDTRF.
           IF WS-STATUS = "05"
               CLOSE LEDTRF
               OPEN OUTPUT LEDTRF
               CLOSE LEDTRF
               OPEN I-O LEDTRF.
044310     IF RUNTIME-ERROR
               IF FLE-LOCKED
040420             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
	       IF IDX-CORRUPT
                   MOVE 40       TO WS-F-ERROR
                   GO TO ZA50.
054240     IF WS-STATUS NOT = "00"
054250         MOVE 40           TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
054260		   PERFORM OPEN-ERROR.
	     OPEN I-O STKALT.
	   IF WS-STATUS = "05"
	       CLOSE STKALT
	       OPEN OUTPUT STKALT
	       CLOSE STKALT
	       OPEN I-O STKALT.
053840	   IF RUNTIME-ERROR
053850         IF FLE-LOCKED
053860             GO TO ZA200
053870         ELSE
053880         IF FLE-LIMIT
053890		   GO TO ZA49
               ELSE
	       IF IDX-CORRUPT
		   MOVE 49	 TO WS-F-ERROR
		   GO TO ZA50.
053900     IF WS-STATUS NOT = "00"
053910	       MOVE 49		 TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
044350		   PERFORM OPEN-ERROR.
044300	     OPEN INPUT STKMEM.
044310     IF RUNTIME-ERROR
               IF FLE-LOCKED
040420             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
	       IF IDX-CORRUPT
                   MOVE 25       TO WS-F-ERROR
                   GO TO ZA50.
044330     IF WS-STATUS NOT = "00"
044340         MOVE 25           TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
044350		   PERFORM OPEN-ERROR.
044480	     OPEN I-O SPARTS.
044490     IF RUNTIME-ERROR
               IF FLE-LOCKED
040420             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
	       IF IDX-CORRUPT
                   MOVE 21       TO WS-F-ERROR
                   GO TO ZA50.
044510     IF WS-STATUS NOT = "00"
044520         MOVE 21           TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
044350		   PERFORM OPEN-ERROR.
054300	     OPEN I-O STOCK.
054310     IF RUNTIME-ERROR
054320         IF FLE-LOCKED
054330             GO TO ZA200
054340         ELSE
054350         IF FLE-LIMIT
054360             GO TO ZA49
054370         ELSE
054380	       IF IDX-CORRUPT
054390             MOVE 22       TO WS-F-ERROR
054400             GO TO ZA50.
054410     IF WS-STATUS NOT = "00"
054420         MOVE 22           TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
054430		   PERFORM OPEN-ERROR.
044300	     OPEN I-O WARHSE.
           IF WS-STATUS = "05"
	       CLOSE WARHSE
	       OPEN OUTPUT WARHSE
	       CLOSE WARHSE
	       OPEN I-O WARHSE.
044310     IF RUNTIME-ERROR
               IF FLE-LOCKED
040420             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
	       IF IDX-CORRUPT
		   MOVE 51	 TO WS-F-ERROR
                   GO TO ZA50.
044330     IF WS-STATUS NOT = "00"
044340	       MOVE 51		 TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
050210		   PERFORM OPEN-ERROR.
044300	     OPEN I-O WSTOCK.
           IF WS-STATUS = "05"
	       CLOSE WSTOCK
	       OPEN OUTPUT WSTOCK
	       CLOSE WSTOCK
	       OPEN I-O WSTOCK.
044310     IF RUNTIME-ERROR
               IF FLE-LOCKED
040420             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
	       IF IDX-CORRUPT
		   MOVE 52	 TO WS-F-ERROR
                   GO TO ZA50.
044330     IF WS-STATUS NOT = "00"
044340	       MOVE 52		 TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
050210		   PERFORM OPEN-ERROR.
039770	     GO TO ZA999.

       COPY ZA49.PRO.

039850 ZA50.
041650	   IF WS-F-ERROR = 3
041660         MOVE "CREDITOR"   TO WS-FILE
041840     ELSE
041850	   IF WS-F-ERROR = 5
041860         MOVE "DEBTOR TRAN"
                                 TO WS-FILE
           ELSE
055350	   IF WS-F-ERROR = 6
055360         MOVE "DEBTOR file"
055370                           TO WS-FILE
055380	   ELSE
055350     IF WS-F-ERROR = 9 
055360         MOVE "GUARANTEE file"
055370                           TO WS-FILE
055380	   ELSE
055350     IF WS-F-ERROR = 12
055360         MOVE "INVOICE file"
055370                           TO WS-FILE
039890	   ELSE
039900     IF WS-F-ERROR = 20
039910         MOVE "SALES ORDER" TO WS-FILE
041840	   ELSE
041850     IF WS-F-ERROR = 21
041860         MOVE "PRODUCTION" TO WS-FILE
           ELSE
           IF WS-F-ERROR = 22
               MOVE "STOCK file" TO WS-FILE
           ELSE
           IF WS-F-ERROR = 25
               MOVE "STOCK MEMO" TO WS-FILE
	   ELSE
041850     IF WS-F-ERROR = 27
041860         MOVE "DEBTOR MEMO"
				 TO WS-FILE
	   ELSE
041850	   IF WS-F-ERROR = 30
041860	       MOVE "ROUTE file" TO WS-FILE
	   ELSE
041850     IF WS-F-ERROR = 40
041860         MOVE "INTEGRATION"
				 TO WS-FILE
           ELSE
041850	   IF WS-F-ERROR = 43
041860	       MOVE "CARDEX file"
				 TO WS-FILE
           ELSE
055350	   IF WS-F-ERROR = 47
055360	       MOVE "DEBALT file"
055370				 TO WS-FILE
	   ELSE
041850	   IF WS-F-ERROR = 49
041860	       MOVE "STOCK DESC INDX"
				 TO WS-FILE
           ELSE
041850	   IF WS-F-ERROR = 51
041860	       MOVE "WAREHOUSE file"
				 TO WS-FILE
           ELSE
041850	   IF WS-F-ERROR = 52
041860	       MOVE "STOCK LOCATION"
				 TO WS-FILE.
039980       DISPLAY "Rebuild " AT 0812
039990                WS-FILE WITH FOREGROUND-COLOR 14.
040000       DISPLAY "Press any key to continue" AT 1012.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
040020 ZA51.
040030       EXIT PROGRAM.
      *
      *    ****    S E T U P   T H E   S C R E E N   G R A P H I C S
      *
040090 ZA60.
040650	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "SupervisorÄ"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.
040660 ZA200.

       COPY LOCKED.PRO.

040730 ZA205.
040740       EXIT PROGRAM.
040750 ZA999.
040760       EXIT.

040780 I-O-ERRORS      SECTION  91.

       COPY ERRORS.PRO.

040950 DISPLAY-FILE-NAME.
056270	   IF WS-F-ERROR = 1
056280         MOVE W02-AUDITF TO WS-FILE
056290         MOVE WS-AUDKEY TO WS-KEY
041800     ELSE
041650	   IF WS-F-ERROR = 3
041660         MOVE W02-CREDIT   TO WS-FILE
041670         MOVE ZERO         TO WS-KEY
               MOVE P-NUMBER     TO WS-KEYX
056300	   ELSE
056350	   IF WS-F-ERROR = 5
056360         MOVE W02-DBTRAN TO WS-FILE
041750         MOVE ZERO         TO WS-KEY
               MOVE TRA-KEY      TO WS-KEYX
056380     ELSE
056390     IF WS-F-ERROR = 6
056400         MOVE W02-DEBTOR TO WS-FILE
041790         MOVE ZERO         TO WS-KEY
               MOVE DEB-ACNO     TO WS-KEYX
	   ELSE
           IF WS-F-ERROR = 9
               MOVE W02-GARTEE   TO WS-FILE
               MOVE ZERO         TO WS-KEY
               MOVE GUA-KEY      TO WS-KEYX
056460     ELSE
056470     IF WS-F-ERROR = 12
056480         MOVE W02-INVOIC   TO WS-FILE
056490         MOVE ZERO         TO WS-KEY
               MOVE DOC-KEY      TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 20
052210         MOVE W02-SORDER   TO WS-FILE
052220         MOVE ZERO         TO WS-KEY
               MOVE SOR-KEY      TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 21
041860         MOVE W02-SPARTS   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
               MOVE PRT-ITEM     TO WS-KEYX
056620	   ELSE
056630     IF WS-F-ERROR = 22
056640         MOVE W02-STOCKF   TO WS-FILE
056650         MOVE ZERO         TO WS-KEY
               MOVE STK-CODE     TO WS-KEYX
041840     ELSE
041850     IF WS-F-ERROR = 25
041860         MOVE W02-STKMEM   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
               MOVE SME-KEY      TO WS-KEYX
041840     ELSE
041850     IF WS-F-ERROR = 27
041860         MOVE W02-DEBMEM   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE DME-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 30
041860	       MOVE W02-ROUTE	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE RTE-ROUTE	 TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 40
041860         MOVE W02-LEDTRF   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE XFR-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 43
041860	       MOVE W02-CARDEX	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE CRD-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 47
041860	       MOVE W02-DEBALT	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE DAL-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 49
041860	       MOVE W02-STKALT	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE STKA-KEY	 TO WS-KEYX
041840	   ELSE
041770	   IF WS-F-ERROR = 51
041780	       MOVE W02-WARHSE TO WS-FILE
041790         MOVE ZERO         TO WS-KEY
	       MOVE WAR-CODE	 TO WS-KEYX
041840	   ELSE
041770	   IF WS-F-ERROR = 52
041780	       MOVE W02-WSTOCK TO WS-FILE
041790         MOVE ZERO         TO WS-KEY
	       MOVE WST-KEY	 TO WS-KEYX.

       COPY DISPERR.PRO.
