      $set LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *   ******    ******   **        **     **  ********  **	   **  *
      *  **    **  **	 **  **        ***   ***  **	    ***    **  *
      *  **	   **	 **  **        ** * * **  **	    ** *   **  *
      *   ******   ********  **        **  *  **  *****     **	*  **  *
      *        **  **	 **  **        **     **  **	    **	 * **  *
      *  **    **  **	 **  **        **     **  **	    **	  ***  *
      *   ******   **	 **  ********  **     **  *******   **	   **  *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     S A L E S	P . O . S .   M O D U L E   M E N U	       *
      *                                                                *
      *     VERSION 8.15.00 - November 2010			       *
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
000020 PROGRAM-ID.     SALMENU.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   JANUARY 1983.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1983 - 2008
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

       01  WS-LOCAL-ID.
	   03  WS-LOCAL-DRIVE  PIC  X(01).
	   03  FILLER	       PIC  X(14) VALUE ":\APACSHR\SORD".
	   03  WS-USER-ID      PIC  X(03).
	   03  FILLER	       PIC  X(04) VALUE ".ORD".

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
001460				 VALUE "SALES (P.O.S.) MODULE".

       01  S02.
	   03  LINE  4 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
	   03	       COLUMN 28 BACKGROUND-COLOR 2 FOREGROUND-COLOR 0
				 VALUE "¿".
	   03  LINE  5 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³  ".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "P.O.S. PROGRAMS".
	   03	       COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "   ³".
	   03  LINE  6 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "1".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Sales            ³".
001470	   03  LINE  7 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "2".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Debtor Receipts  ³".
001470	   03  LINE  8 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "3".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Programming      ³".
001470	   03  LINE  9 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "4".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Enquiries        ³".
001570	   03  LINE 10 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "5".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Sales Statistics ³".
001570	   03  LINE 11 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "6".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Lay By's         ³".
001500	   03  LINE 12 COLUMN  7 BACKGROUND-COLOR 2
001500*   03  LINE 11 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "0".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Exit	          ³".
	   03  LINE 13 COLUMN  7 BACKGROUND-COLOR 2
      *	   03  LINE 12 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "À".
	   03	       COLUMN  8 BACKGROUND-COLOR 2 FOREGROUND-COLOR 0
				 VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S05.
	   03  LINE  6 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
	   03	       COLUMN 51 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
				 VALUE "¿".
	   03  LINE  7 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³    ".
	   03	       COLUMN 31 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "CASH SALES (POS)".
	   03	       COLUMN 47 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE "    ³".
	   03  LINE  8 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "1".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Cash Sale (Stock)    ³".
001470	   03  LINE  9 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "2".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Open Cash Drawer     ³".
001570	   03  LINE 10 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "3".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Balancing Report     ³".
001570	   03  LINE 11 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "4".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Adjust Float         ³".
001570	   03  LINE 12 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "5".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
004680				 VALUE " Day/Month/Year-End   ³".
001570	   03  LINE 13 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "6".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Cash Refund (Stock)  ³".
001570	   03  LINE 14 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "7".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Initialize Drawer    ³".
001570	   03  LINE 15 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "8".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Cash Sale Statistics ³".
001570	   03  LINE 16 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "A".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Petty Cash Voucher   ³".
001570	   03  LINE 17 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "B".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Picking Slips	      ³".
001570	   03  LINE 18 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "0".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Exit	              ³".
	   03  LINE 19 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "À".
	   03	       COLUMN 27 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
				 VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S07.
	   03  LINE  6 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
	   03	       COLUMN 45 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
				 VALUE "¿".
	   03  LINE  7 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³    ".
	   03	       COLUMN 31 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "ENQUIRIES".
	   03	       COLUMN 40 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE "     ³".
	   03  LINE  8 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "1".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Debtor Account ³".
001470	   03  LINE  9 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "2".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Stock Record   ³".
001570	   03  LINE 10 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "0".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Exit	        ³".
001500	   03  LINE 11 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "À".
	   03	       COLUMN 27 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
				 VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

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

       COPY FUNCTION.SCR.

       COPY ERROR.SCR.

011433 COPY CLEAR.CRT.

       BA000	       SECTION.
       BA00.
	     MOVE 5		 TO SHADE-ROW.
	     MOVE 9		 TO SHADE-COL.
	     MOVE 20		 TO SHADE-WIDTH.
	     MOVE 9		 TO SHADE-LINES.
	     DISPLAY S02.
	   IF LS0-LEV < 2
	       DISPLAY "(N/A)" AT 0823
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2.
	       DISPLAY "s (N/A)" AT 1121
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.
       BA02.
	     MOVE 5		 TO TOP-ROW.
	     MOVE 11		 TO BOTTOM-ROW.
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
	       IF (WS-OPTION < 0 OR > 6)
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
	       MOVE "0"		 TO WS-OPTION.
	   GO TO BA18.

       BA16.
	     PERFORM CLEAR-BLOCK.
	   IF WS-OPTION = "0"
	       MOVE 11		 TO SCREEN-LIN
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
	       MOVE 10		 TO SCREEN-LIN.
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
	       PERFORM IA000
	   ELSE
	   IF WS-OPTION = "2"
	       PERFORM JA000
	   ELSE
	   IF WS-OPTION = "3"
	       PERFORM KA000
	   ELSE
	   IF WS-OPTION = "4"
	       PERFORM LA000
	   ELSE
	   IF WS-OPTION = "5"
	       PERFORM NA000
	   ELSE
	   IF WS-OPTION = "6"
	       PERFORM OA000.
	     PERFORM RESTORE-SCREEN.
	     MOVE BLOCK-DETAIL1  TO BLOCK-DETAIL.
	     MOVE SHADOW-DETAIL1 TO SHADOW-DETAIL.
	     MOVE CRT-DETAIL1	 TO CRT-DETAIL.
	     GO TO BA05.
       BA999.
	     EXIT.

      /
      *    ****    C A S H   S A L E S	 ( P O S )
      *
       IA000	       SECTION 50.
       IA00.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 28		 TO SHADE-COL.
	     MOVE 24		 TO SHADE-WIDTH.
	     MOVE 13		 TO SHADE-LINES.
	     DISPLAY S05.
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
		   IF WS-SYS-ID = "MDE"
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
		   MOVE "INV\DTPPOS"
				 TO PRG-NAME
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
	       ELSE
	       IF LS-INSTR = "C"
		   MOVE "POS\CSHBAL"
				 TO PRG-NAME
		   PERFORM AA100
		   MOVE "1"	 TO WS-OPTION
		   GO TO IA18
	       ELSE
	       IF LS-INSTR = "S"
		   MOVE "POS\STFREF"
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
       JA000	       SECTION 51.
       JA00.
	   IF LS-DSKDRV = "L"
	       MOVE "Not available on Local drive"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO JA999.
	     MOVE "DTP\DTP012"	 TO PRG-NAME.
	     PERFORM AA100.

       JA999.
	     EXIT.

      /
      *    ****    P R O G R A M M I N G   K E Y S
      *
       KA000	       SECTION 51.
       KA00.
	   IF LS-DSKDRV = "L"
	       MOVE "Not available on Local drive"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO KA999.
	   IF LS0-LEV < 2
	       MOVE "MNU\NOPMENU"
				 TO PRG-NAME
	   ELSE
	       MOVE "POS\POS004" TO PRG-NAME.
	     PERFORM AA100.

       KA999.
	     EXIT.

      /
      *    ****    E N Q U I R I E S
      *
       LA000	       SECTION 52.
       LA00.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 28		 TO SHADE-COL.
	     MOVE 18		 TO SHADE-WIDTH.
	     MOVE 5		 TO SHADE-LINES.
	     DISPLAY S07.
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
      *    ****    S A L E S   S T A T I S T I C S
      *
       NA000	       SECTION 53.
       NA00.
	   IF LS-DSKDRV = "L"
	       MOVE "Not available on Local drive"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO NA999.
	     MOVE "STP\STPTOP"	 TO PRG-NAME.
	     PERFORM AA100.

       NA999.
	     EXIT.
      /
      * 	   L A Y   B Y ' S
      *
       OA000	       SECTION 53.
       OA00.
	   IF LS0-LEV < 2
	       MOVE "MNU\NOPMENU"
				 TO PRG-NAME
	   ELSE
	       MOVE "SLP\LBP001" TO PRG-NAME.
	    PERFORM AA100.

       OA999.
	    EXIT.

      /
038140 ZA000-INIT              SECTION 90.
038150 ZA000-OPEN.
038160	     PERFORM ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
      *
      *    ****    S E T   U P   T H E   F U N C T I O N   K E Y S
      *
	     MOVE 1		 TO USER-ACTION
				    USER-SETTING.
      *
      *    ESC, F1 to F10 keys
      *
	     MOVE ZERO		 TO USER-NUMBER.
	     MOVE 11		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *
      *    PAGE-UP AND PAGE-DOWN KEYS
      *
	     MOVE 53		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *
      *    ****    A C T I V A T E   M O U S E
      *
	     MOVE 64		 TO MOUSE-FUNC.
	     MOVE 1		 TO MOUSE-PARAM.
	     CALL X"AF" USING MOUSE-FUNC
			      MOUSE-PARAM.
	   IF MOUSE-FUNC NOT = 255
	       MOVE "Y"		 TO MOUSE-ATTACHED
	   ELSE
	       GO TO ZA00A.
      *
      *    ****    S E T   M O U S E   K E Y   T O   A C T
      * 	   A S	 F U N C T I O N   K E Y
      *
	     MOVE 3		 TO USER-ACTION.
	     MOVE 27		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *
      *    ****    P A R A M E T E R   F I L E
      *
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
	     MOVE "AUDITF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-AUDITF.
	     MOVE "CARDEX"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CARDEX.
	     MOVE "CREDIT"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CREDIT.
	     MOVE "DBTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DBTRAN.
      *    *************************************************************
      *    *** D E B T O R   A L T E R N A T E	 I N D E X   F I L E ***
      *    *************************************************************
	     MOVE "DEBALT"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBALT.
	     MOVE "DEBMEM"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBMEM.
	     MOVE "DEBTOR"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBTOR.
	     MOVE "GARTEE"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-GARTEE.
	     MOVE "INVOIC"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-INVOIC.
	     MOVE "LEDTRF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-LEDTRF.
	     MOVE "SORDER"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SORDER.
	     MOVE "SPARTS"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SPARTS.
      *    *************************************************************
      *    ****	S T O C K   A L T E R N A T E	I N D E X   F I L E ****
      *    *************************************************************
	     MOVE "STKALT"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STKALT.
	     MOVE "STKMEM"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STKMEM.
	     MOVE "STOCKF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STOCKF.
	     MOVE "WARHSE"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-WARHSE.
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
	       MOVE 27		 TO WS-F-ERROR
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

039780 ZA49.
039790       DISPLAY "Too many files OPEN" AT 0812
039800                WITH FOREGROUND-COLOR 14.
039810       DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
039820       DISPLAY "Press any key to continue" AT 1212.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
039840	     GO TO ZA205.
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
040670       DISPLAY "Files locked - Try later" AT 2312
040680                WITH FOREGROUND-COLOR 14
040690               " " WS-STATUS WITH FOREGROUND-COLOR 15.
040700       DISPLAY "Press any key" AT 2512 
040710		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
040730 ZA205.
040740       EXIT PROGRAM.
040750 ZA999.
040760       EXIT.

040780 I-O-ERRORS      SECTION  91.
040790 OPEN-ERROR.
040800       DISPLAY CLR-SCREEN.
040810       DISPLAY "Open error" AT 0812 WITH FOREGROUND-COLOR 14.
040820       PERFORM DISPLAY-FILE-NAME.
040830
040840 READ-ERROR.
040850       DISPLAY CLR-SCREEN.
040860       DISPLAY "Read error" AT 0812 WITH FOREGROUND-COLOR 14.
040870       PERFORM DISPLAY-FILE-NAME.
040880
040890 WRITE-ERROR.
040900       DISPLAY CLR-SCREEN.
040910       DISPLAY "Write error" AT 0812 WITH FOREGROUND-COLOR 14.
040920       PERFORM DISPLAY-FILE-NAME.
040930

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
041350	   IF WS-STATUS = "10"
041360         MOVE "End of FILE" TO WS-STAT-MESSAGE
041370     ELSE
041380     IF WS-STATUS = "22"
041390         MOVE "Duplicate record number" TO WS-STAT-MESSAGE
041400     ELSE
041410     IF WS-STATUS = "23"
041420         MOVE "Invalid record number" TO WS-STAT-MESSAGE
041430     ELSE
041440     IF WS-STATUS = "24"
041450         MOVE "DISK full" TO WS-STAT-MESSAGE
041460     ELSE
041470     IF WS-STATUS = "30"
041480         MOVE "DISK error" TO WS-STAT-MESSAGE
041490     ELSE
041500     IF WS-STATUS = "94"
041510         MOVE "FILE locked" TO WS-STAT-MESSAGE.
041520       DISPLAY "File - " AT 1012 WS-FILE WITH FOREGROUND-COLOR 11.
041530       DISPLAY "Status " AT 1212 
041540                WS-STATUS WITH FOREGROUND-COLOR 11
041550               ": " WS-STAT-MESSAGE WITH FOREGROUND-COLOR 14.
041560     IF WS-STATUS NOT = "94" 
041570         DISPLAY "Key    " AT 1412 
041580                  WS-KEY WITH FOREGROUND-COLOR 11
                        " " WS-KEYX WITH FOREGROUND-COLOR 11
041590         DISPLAY "Reverse backup or contact program Support"
041600                  AT 1612.
041610         DISPLAY "Please make a note of these details" AT 1812.
041620       STOP RUN.
