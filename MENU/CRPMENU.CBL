      $set LINKCOUNT"384"
      ******************************************************************
      *                                                                *
      *   ******   *******   *******   **     **  ********  **	   **  *
      *  **    **  **	 **  **	   **  ***   ***  **	    ***	   **  *
      *  **	   **	 **  **	   **  ** * * **  **	    ** *   **  *
      *  **	   *******   *******   **  *  **  *****	    **	*  **  *
      *  **	   **	**   **	       **     **  **	    **	 * **  *
      *  **    **  **	 **  **	       **     **  **	    **	  ***  *
      *   ******   **	 **  **        **     **  *******   **	   **  *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     C R E D I T O R   M O D U L E   M E N U   P R O G R A M    *
      *                                                                *
      *     VERSION 8.14.02 - January 2010			       *
      * 							       *
      ******************************************************************
      * 							       *
      *  Aug 2008	- New file (STKALT) - Stock alternate index    *
      * 		  included for lookups, using any word con-    *
      * 		  tained in the Stock description.	       *
      * 							       *
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		    VERSIONS OF THE APAC SYSTEM		       Ё
      *    фммммммммммммммммммммммммммммммммммммммммммммммммммммммммммм╣
      *    Ё While developing this system the requirements and budgets Ё
      *    Ё (originally this was a propriety package) for various     Ё
      *    Ё businesses necessitated that at least 4 separate systems  Ё
      *    Ё be produced. As the maintenance and amendments to the 4   Ё
      *    Ё different systems would require man hours that would not  Ё
      *    Ё enable me to do much development, it was decided to use   Ё
      *    Ё one system with the various requirements controlled by    Ё
      *    Ё the various menu programs using a version indicator. This Ё
      *    Ё indicator is located in the second record of the SYSTEM   Ё
      *    Ё file SEC-LEV, which is in the SEC-MODULES. This indicator Ё
      *    Ё together with all the other indicators in SEC-MODULES, is Ё
      *    Ё moved to WS0-MODULES in BUP000 or BUP001 and then passed  Ё
      *    Ё via the LINKAGE SECTION from program to program in  the   Ё
      *    Ё LS0-MODULES group. The other indicators in this group are Ё
      *    Ё used to show which modules of the system are installed.   Ё
      *    фмммммммммммммммммммямммммммммммммммммммяммммммммммммммммммм╣
      *    Ё	SYSTEM FILE    Ё      CHAIN.WS	   Ё	 CHAIN.LS      Ё
      *    цдддддддддддддддддддедддддддддддддддддддеддддддддддддддддддд╢
      *	   Ё  03  SEC-MODULES. Ё  03  WS0-MODULES. Ё  03  LS0-MODULES. Ё
      *    Ё	  05  SEC-DTP  Ё      05  WS0-DTP  Ё	  05  LS0-DTP  Ё
      *    Ё	  05  SEC-STP  Ё      05  WS0-STP  Ё	  05  LS0-STP  Ё
      *    Ё	  05  SEC-HPD  Ё      05  WS0-HPD  Ё	  05  LS0-HPD  Ё
      *    Ё	  05  SEC-CRP  Ё      05  WS0-CRP  Ё	  05  LS0-CRP  Ё
      *    Ё	  05  SEC-GLP  Ё      05  WS0-GLP  Ё	  05  LS0-GLP  Ё
      *    Ё	  05  SEC-POS  Ё      05  WS0-POS  Ё	  05  LS0-POS  Ё
      *    Ё	  05  SEC-JCP  Ё      05  WS0-JCP  Ё	  05  LS0-JCP  Ё
      *    Ё	  05  SEC-VHP  Ё      05  WS0-VHP  Ё	  05  LS0-VHP  Ё
      *    Ё	  05  SEC-WAG  Ё      05  WS0-WAG  Ё	  05  LS0-WAG  Ё
      *    Ё	  05  SEC-SAL  Ё      05  WS0-SAL  Ё	  05  LS0-SAL  Ё
      *    Ё	  05  SEC-LEV  Ё      05  WS0-LEV  Ё	  05  LS0-LEV  Ё
      *    Ё	  05  SEC-WHS  Ё      05  WS0-WHS  Ё	  05  LS0-WHS  Ё
      *    Ё	  05  SEC-RTE  Ё      05  WS0-RTE  Ё	  05  LS0-RTE  Ё
      *	   Ё	  05  FILLER   Ё		   Ё		       Ё
      *    юдддддддддддддддддддадддддддддддддддддддаддддддддддддддддддды
      *
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     CRPMENU.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   JANUARY 1983.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1983 - 2010
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

000160 COPY CREDIT.SL.

000180 COPY CRDMEM.SL.

000180 COPY CRTRAN.SL.

       COPY LEDTRF.SL.

       COPY PDCHEQ.SL.

000260 COPY PORDER.SL.

000300 COPY PURDEX.SL.

       COPY STKALT.SL.

000140 COPY STKMEM.SL.

000200 COPY STOCK.SL.

       COPY WARHSE.SL.

       COPY WSTOCK.SL.

      /
000330 DATA DIVISION.
000340 FILE SECTION.

       COPY APACFIDS.FDE.

000160 COPY CREDIT.FDE.

000180 COPY CRDMEM.FDE.

000180 COPY CRTRAN.FDE.

       COPY LEDTRF.FDE.

       COPY PDCHEQ.FDE.

000260 COPY PORDER.FDE.

000300 COPY PURDEX.FDE.

       COPY STKALT.FDE.

000140 COPY STKMEM.FDE.

000200 COPY STOCK.FDE.

       COPY WARHSE.FDE.

       COPY WSTOCK.FDE.

      /
000560 WORKING-STORAGE SECTION.
       77  WS-COPYRIGHT    PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
000610 77  WS-S1	   PIC	9(04)	 COMP-5.
000620 77  WS-S2           PIC  9(04)    COMP-5.
000630 77  WS-S3           PIC  9(04)    COMP-5.
000640 77  WS-S4           PIC  9(04)    COMP-5.
       77  WS-TEMP-LIN	   PIC	9(02)	 COMP-5.
       77  WS-OPTION	   PIC	X(01).
       77  PRG-NAME	   PIC	X(12) VALUE SPACES.
000420 77  WS-PARKEY	   PIC	9(04)	 COMP-5 VALUE 1.
000430 77  WS-CTRKEY       PIC  9(04)    COMP-5 VALUE 1.
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.

001350 01  WS-CRT-LINES.
001380	   03  WS-TOP-LNE2.
001390	       05  WS-TCR   PIC  X(80) VALUE "зддддддддддддддддддддддддд
      -        "ддддддддддддддддддддддддддддддддддддддддддддддддддддд©".
001400	   03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
001410         05  FILLER      PIC  X(01).
001420         05  WS-TOP-COMP PIC  X(40).
001430	       05  FILLER      PIC  X(23).
	       05  WS-WRKHD    PIC  X(11).
001430	       05  FILLER      PIC  X(01).
	       05  WS-WRKST    PIC  X(03).
001430	       05  FILLER      PIC  X(01).
001460	   03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "Ё".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "╟".
	       05  FILLER      PIC  X(01) VALUE "Ё".

001840 COPY WS.WS.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002040 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CREDIT.ID.

       COPY CRDMEM.ID.

       COPY CRTRAN.ID.

       COPY LEDTRF.ID.

       COPY PARAM.ID.

       COPY PDCHEQ.ID.

       COPY PORDER.ID.

000300 COPY PURDEX.ID.

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
001450	   03  LINE  2 COLUMN 29 BACKGROUND-COLOR 1
				 FOREGROUND-COLOR 7 HIGHLIGHT
001460				 VALUE "CREDITOR CONTROL MODULE".
       01  S02.
	   03  LINE  4 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "зддддддддддддддддддд".
	   03	       COLUMN 27 BACKGROUND-COLOR 2 FOREGROUND-COLOR 0
				 VALUE "©".
	   03  LINE  5 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "Ё ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "CREDITOR PROGRAMS".
	   03	       COLUMN 26 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Ё".
	   03  LINE  6 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "Ё ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "1".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Maintenance	 Ё".
001470	   03  LINE  7 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "Ё ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "2".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Transactions	 Ё".
001470	   03  LINE  8 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "Ё ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "3".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Reports    	 Ё".
001470	   03  LINE  9 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "Ё ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "4".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Transactions(GL)Ё".
001570	   03  LINE 10 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "Ё ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "5".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Export Options  Ё".
001500	   03  LINE 11 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "Ё ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "6".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Utilities       Ё".
001500	   03  LINE 12 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "Ё ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "7".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Enquiries       Ё".
001500	   03  LINE 13 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "Ё ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "8".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Purch. Tracking Ё".
001500	   03  LINE 14 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "Ё ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "0".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Exit	         Ё".
	   03  LINE 15 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "ю".
	   03	       COLUMN  8 BACKGROUND-COLOR 2 FOREGROUND-COLOR 0
				 VALUE "ддддддддддддддддддды".

       01  S03.
	   03  LINE  6 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "зддддддддддддддддддддддд".
	   03	       COLUMN 50 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
				 VALUE "©".
	   03  LINE  7 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "Ё ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "CREDITOR MAINTENANCE".
	   03	       COLUMN 48 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE "  Ё".
	   03  LINE  8 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "Ё ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "1".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Create New Account  Ё".
001470	   03  LINE  9 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "Ё ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "2".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Amend Account RecordЁ".
001570	   03  LINE 10 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "Ё ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "3".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Delete Account Rec. Ё".
001570	   03  LINE 11 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "Ё ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "4".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Amend Account No.   Ё".
001570	   03  LINE 12 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "Ё ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "5".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Memo/Remarks Maint  Ё".
001570	   03  LINE 13 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "Ё ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "6".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Age Accounts        Ё".
001570	   03  LINE 14 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "Ё ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "7".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Account Enquiry     Ё".
001570	   03  LINE 15 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "Ё ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "0".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Exit	             Ё".
001570	   03  LINE 16 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "ю".
	   03	       COLUMN 27 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
				 VALUE "ддддддддддддддддддддддды".

       01  S04.
	   03  LINE  6 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "здддддддддддддддддддддд".
	   03	       COLUMN 49 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
				 VALUE "©".
	   03  LINE  7 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "Ё".
	   03	       COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "CREDITOR TRANSACTIONS".
	   03	       COLUMN 48 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Ё".
	   03  LINE  8 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "Ё ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "1".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Invoices (STOCK)   Ё".
001470	   03  LINE  9 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "Ё ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "2".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Debit Note (STOCK) Ё".
001570	   03  LINE 10 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "Ё ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "3".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Journal Entries    Ё".
001570	   03  LINE 11 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "Ё ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "4".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Transfers          Ё".
001570	   03  LINE 12 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "Ё ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "5".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Purchase Orders    Ё".
001570	   03  LINE 13 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "Ё ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "6".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Invoices (Analysed)Ё".
001570	   03  LINE 14 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "Ё ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "7".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Dt Notes (Analysed)Ё".
001570	   03  LINE 15 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "Ё ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "8".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Discount Received  Ё".
001570	   03  LINE 16 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "Ё ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "A".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Cheques/Payments   Ё".
001570	   03  LINE 17 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "Ё ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "B".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Creditor Enquiry   Ё".
001570	   03  LINE 18 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "Ё ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "C".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Price Labels       Ё".
001570	   03  LINE 19 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "Ё ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "D".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Enquiries (Orders) Ё".
001570	   03  LINE 20 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "Ё ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "0".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Exit	            Ё".
	   03  LINE 21 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "ю".
	   03	       COLUMN 27 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
				 VALUE "дддддддддддддддддддддды".

       01  S05.
	   03  LINE  6 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "зддддддддддддддддддддддд".
	   03	       COLUMN 50 FOREGROUND-COLOR 0 BACKGROUND-COLOR 3
				 VALUE "©".
	   03  LINE  7 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "Ё   ".
	   03	       COLUMN 30 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "CREDITOR REPORTS".
	   03	       COLUMN 46 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE "    Ё".
	   03  LINE  8 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "Ё ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "1".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Creditors Listing   Ё".
001470	   03  LINE  9 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "Ё ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "2".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Creditors Cash Flow Ё".
001570	   03  LINE 10 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "Ё ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "3".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Purchase Journal    Ё".
001570	   03  LINE 11 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "Ё ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "4".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Remittance Advices  Ё".
001570	   03  LINE 12 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "Ё ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "5".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Outstanding Orders  Ё".
001570	   03  LINE 13 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "Ё ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "6".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Post dated Cheques  Ё".
001570	   03  LINE 14 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "Ё ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "7".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Address Labels      Ё".
001570	   03  LINE 15 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "Ё ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "8".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Discount Listing    Ё".
001570	   03  LINE 16 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "Ё ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "A".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Purchase Tracking   Ё".
001570	   03  LINE 17 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "Ё ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "B".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Account Enquiry     Ё".
001570	   03  LINE 18 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "Ё ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "0".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Exit	             Ё".
	   03  LINE 19 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "ю".
	   03	       COLUMN 27 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
				 VALUE "ддддддддддддддддддддддды".

       01  S06.
	   03  LINE  6 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "здддддддддддддддддддддд".
	   03	       COLUMN 49 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
				 VALUE "©".
	   03  LINE  7 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "Ё ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "CRED/GL TRANSACTIONS".
	   03	       COLUMN 48 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Ё".
	   03  LINE  8 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "Ё ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "1".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Invoices           Ё".
001470	   03  LINE  9 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "Ё ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "2".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Debit Notes        Ё".
001570	   03  LINE 10 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "Ё ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "3".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Cheques Issued     Ё".
001570	   03  LINE 11 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "Ё ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "4".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Post Dated Cheques Ё".
001570	   03  LINE 12 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "Ё ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "5".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Electronic Payment Ё".
001570	   03  LINE 13 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "Ё ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "6".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Creditor Enquiry   Ё".
001570	   03  LINE 14 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "Ё ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "7".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " G/L Account EnquiryЁ".
001570	   03  LINE 15 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "Ё ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "0".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Exit	            Ё".
	   03  LINE 16 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "ю".
	   03	       COLUMN 27 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
				 VALUE "дддддддддддддддддддддды".

       01  S07.
	   03  LINE  6 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "зддддддддддддддддддддддд".
	   03	       COLUMN 50 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
				 VALUE "©".
	   03  LINE  7 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "Ё   ".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "CREDITOR UTILITIES".
	   03	       COLUMN 47 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE "   Ё".
	   03  LINE  8 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "Ё ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "1".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Clear Balances      Ё".
001470	   03  LINE  9 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "Ё ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "2".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Re-age a TransactionЁ".
001570	   03  LINE 10 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "Ё ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "3".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Amend Cash Flow     Ё".
001570	   03  LINE 11 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "Ё ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "4".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Delete Orders       Ё".
001570	   03  LINE 12 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "Ё ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "5".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Enquiries           Ё".
001570	   03  LINE 13 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "Ё ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "0".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Exit	             Ё".
	   03  LINE 14 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "ю".
	   03	       COLUMN 27 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
				 VALUE "ддддддддддддддддддддддды".

       COPY ERROR.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
       AA000	       SECTION.
       AA00.
	     PERFORM ZA000-INIT.
	     DISPLAY CLR-L1-2.
	     DISPLAY S01.
003240	   IF LS0-CRP NOT = 2
	       MOVE "Not Installed"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	   ELSE
003240	   IF LS0-CRED = "N"
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
043290	   IF LS0-STP = 2
043300         CLOSE STOCK
		     STKALT
                     STKMEM
043330               PORDER.
	     CLOSE CTRANS
043360		   CREDIT
		   CRDMEM
		   LEDTRF
		   PDCHEQ
		   PURDEX
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
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	   SAVE-SCREEN /-2/-3  and  RESTORE-SCREEN /-2/-3      Ё
      *    фммммммммммммммммммммммммммммммммммммммммммммммммммммммммммм╣
      *    Ё			  SCREEN-SHADOW 		       Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё To routine is used to display a shadow down the right and Ё
      *    Ё along the bottom of a pop-up box. The parameters that are Ё
      *    Ё required:						       Ё
      *	   Ё	       SHADE-ROW   - Top line of the box + 1.	       Ё
      *	   Ё	       SHADE-COL   - Left line of box + 2.	       Ё
      *	   Ё	       SHADE-WIDTH - Width of the box - 2.	       Ё
      *	   Ё	       SHADE-LINES - Hight of box - 1.		       Ё
      *    фммммммммммммммммммммммммммммммммммммммммммммммммммммммммммм╣
      *    Ё			  ERROR-MESSAGE 		       Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё To display an error message with " - Press ANY key" at    Ё
      *    Ё the end; use PERFORM ERROR-MESSAGE.		       Ё
      *    Ё							       Ё
      *    Ё To display just the error message; use PERFORM	       Ё
      *    Ё ERROR-LENGTH THRU ERROR-EXIT.			       Ё
      *    Ё							       Ё
      *    Ё To display the error message higher or lower (default is  Ё
      *    Ё line 13) firstly use PERFORM ERROR-SETUP THRU ERROR-POS   Ё
      *    Ё or PERFORM ERROR-LENGTH THRU ERROR-POS. Move the line     Ё
      *    Ё number to be used to SLIN and then PERFORM ERROR-DISPLAY  Ё
      *    Ё THRU ERROR-EXIT.					       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       COPY FUNCTION.SCR.

      /    *************************************************************
      *    ****   T H I S   R O U T I N E   I S   U S E D   T O
      * 	  D I S P L A Y   E R R O R   M E S S A G E S
      *    *************************************************************
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё			  ERROR-MESSAGE 		       Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё To display an error message with " - Press ANY key" at    Ё
      *    Ё the end; use PERFORM ERROR-MESSAGE.		       Ё
      *    Ё							       Ё
      *    Ё To display just the error message; use PERFORM	       Ё
      *    Ё ERROR-LENGTH THRU ERROR-EXIT.			       Ё
      *    Ё							       Ё
      *    Ё To display the error message higher or lower (default is  Ё
      *    Ё line 13) firstly use PERFORM ERROR-SETUP THRU ERROR-POS   Ё
      *    Ё or PERFORM ERROR-LENGTH THRU ERROR-POS. Move the line     Ё
      *    Ё number to be used to SLIN and then PERFORM ERROR-DISPLAY  Ё
      *    Ё THRU ERROR-EXIT.					       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       COPY ERROR.SCR.

      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	      ERASE SCREEN FROM SPECIFIED LOCATION	       Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё Uses CPOS (CLIN,CCOL) as starting position and increases  Ё
      *    Ё CLIN by 1 until CLIN reaches the line allocated to exit.  Ё
      *    Ё The screen is cleared with Column 1 and 80 containing "Ё" Ё
      *    Ё and columns 2 to 79 containing spaces.		       Ё
      *    Ё  eg.						       Ё
      *    Ё	  MOVE 0801	 TO CPOS.			       Ё
      *    Ё	  PERFORM ERASE-SCREEN-LOOP UNTIL CLIN > 19.	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

011433 COPY CLEAR.CRT.

       BA000	       SECTION 50.
       BA00.
	     MOVE 5		 TO SHADE-ROW.
	     MOVE 9		 TO SHADE-COL.
	     MOVE 19		 TO SHADE-WIDTH.
	     MOVE 11		 TO SHADE-LINES.
	     DISPLAY S02.
	   IF LS0-LEV < 2
	       DISPLAY ".(N/A)" AT 1021
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.
       BA02.
	     MOVE 5		 TO TOP-ROW.
	     MOVE 13		 TO BOTTOM-ROW.
	     MOVE 4		 TO SCREEN-LIN.
	     MOVE 9		 TO SCREEN-COL.
	     MOVE 17		 TO STRING-LENGTH.
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
	       IF WS-OPTION < 0 OR > 8
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
	       MOVE "0"		 TO WS-OPTION.
	   GO TO BA18.

       BA16.
	     PERFORM CLEAR-BLOCK.
	   IF WS-OPTION = "0"
	       MOVE 13		 TO SCREEN-LIN
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
	       MOVE 12		 TO SCREEN-LIN.
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
	       PERFORM JA000.
	     PERFORM RESTORE-SCREEN.
	     MOVE BLOCK-DETAIL1  TO BLOCK-DETAIL.
	     MOVE SHADOW-DETAIL1 TO SHADOW-DETAIL.
	     MOVE CRT-DETAIL1	 TO CRT-DETAIL.
	     GO TO BA05.
       BA999.
	     EXIT.
      /
      *    ****    C R E D I T O R   M A I N T E N A N C E
      *
       CA000	       SECTION 50.
       CA00.
	   IF LS-DSKDRV = "L"
	       MOVE "Not available on Local drive"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO CA999.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 28		 TO SHADE-COL.
	     MOVE 23		 TO SHADE-WIDTH.
	     MOVE 10		 TO SHADE-LINES.
	     DISPLAY S03.
	   IF LS0-LEV < 3
	       DISPLAY "N/A" AT 1147
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 3
	       DISPLAY "Mnt N/A" AT 1243
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 3.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.
	     MOVE 7		 TO TOP-ROW.
	     MOVE 14		 TO BOTTOM-ROW.
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
	       IF WS-OPTION < 0 OR > 7
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
	       MOVE "0"		 TO WS-OPTION.
	     GO TO CA18.

       CA16.
	     PERFORM CLEAR-BLOCK.
	   IF WS-OPTION = "0"
	       MOVE 14		 TO SCREEN-LIN
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
	       MOVE 13		 TO SCREEN-LIN.
	     PERFORM SAVE-ATTR.
	     PERFORM MARK-BLOCK.

       CA18.
	   IF WS-OPTION = "0"
	       GO TO CA999.
	   IF WS-OPTION = "1"
	       MOVE "CRP\CRP001" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "2"
	       MOVE "CRP\CRP002" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "3"
	       MOVE "CRP\CRP003" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "4"
	       IF LS0-LEV < 3
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		   Not available in this version	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
		   MOVE "MNU\NOPMENU" TO PRG-NAME
	       ELSE
		   MOVE "CRP\CRP004" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "5"
	       IF LS0-LEV < 3
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		   Not available in this version	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
		   MOVE "MNU\NOPMENU" TO PRG-NAME
	       ELSE
		   MOVE "CRP\CRP005" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "6"
	       MOVE "CRP\CRP006" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "7"
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё			     Enquiries			       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	       MOVE "CRP\CRPENQ" TO PRG-NAME.
	     PERFORM AA100.
	     DISPLAY MENU-INSTRUCT.
	     GO TO CA05.
       CA999.
	     EXIT.
      /
      *    ****    C R E D I T O R   T R A N S A C T I O N S
      *
       DA000	       SECTION 51.
       DA00.
	   IF LS-DSKDRV = "L"
	       MOVE "Not available on Local drive"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO DA999.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 28		 TO SHADE-COL.
	     MOVE 22		 TO SHADE-WIDTH.
	     MOVE 15		 TO SHADE-LINES.
	     DISPLAY S04.
	   IF LS0-LEV < 3
	       DISPLAY "Enq. (Ord) (N/A)  " AT 1930
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 3.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.
	     MOVE 7		 TO TOP-ROW.
	     MOVE 19		 TO BOTTOM-ROW.
	     MOVE 6		 TO SCREEN-LIN.
	     MOVE 28		 TO SCREEN-COL.
	     MOVE 20		 TO STRING-LENGTH.
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
	       IF (WS-OPTION < 0 OR > 8) AND
		  (WS-OPTION < "A" OR > "D") AND
		  (WS-OPTION NOT = "^" AND "&" AND "*" AND "@")
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
	       MOVE "B"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 17
	       MOVE "C"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 18
	       MOVE "D"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 19
	       MOVE "0"		 TO WS-OPTION.
	   GO TO DA18.

       DA16.
	     PERFORM CLEAR-BLOCK.
	   IF WS-OPTION = "0"
	       MOVE 19		 TO SCREEN-LIN
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
	   IF WS-OPTION = "6" OR "^"
	       MOVE 12		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "7" OR "&"
	       MOVE 13		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "8" OR "*"
	       MOVE 14		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "A" OR "@"
	       MOVE 15		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "B"
	       MOVE 16		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "C"
	       MOVE 17		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "D"
	       MOVE 18		 TO SCREEN-LIN.
	     PERFORM SAVE-ATTR.
	     PERFORM MARK-BLOCK.

       DA18.
	   IF WS-OPTION = "0"
	       GO TO DA999.
	   IF WS-OPTION = "1"
	       MOVE "CRP\CRP011" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "2"
	       MOVE "CRP\CRP012" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "3"
	       MOVE "CRP\CRP013" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "4"
	       MOVE "CRP\CRP014" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "5"
	       MOVE "CRP\CRP015" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "6"
	       MOVE "CRP\CRP016" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "^"
	       MOVE "N" 	 TO LS-OK
	       MOVE "CRP\CRP016" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "7"
	       MOVE "CRP\CRP017" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "&"
	       MOVE "N" 	 TO LS-OK
	       MOVE "CRP\CRP017" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "8"
	       MOVE "CRP\CRP018" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "*"
	       MOVE "N" 	 TO LS-OK
	       MOVE "CRP\CRP018" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "A"
	       MOVE "CRP\CRP019" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "@"
	       MOVE "N" 	 TO LS-OK
	       MOVE "CRP\CRP019" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "B"
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё			     Enquiries			       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	       MOVE "CRP\CRPENQ" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "C"
	       MOVE "CRP\CRP020" TO PRG-NAME
	   ELSE
	   IF LS0-LEV < 3
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		   Not available in this version	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	       MOVE "MNU\NOPMENU" TO PRG-NAME
	   ELSE
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё			Enquiries (Orders)		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	       MOVE "STP\STPORD" TO PRG-NAME
	       PERFORM AA100
	       MOVE "STP\ORDPRN" TO PRG-NAME.
	     PERFORM AA100.
	     DISPLAY MENU-INSTRUCT.
	     GO TO DA05.

       DA999.
	     EXIT.
      /
      *    ****   C R E D I T O R S   R E P O R T S
      *
       EA000	       SECTION 52.
       EA00.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 28		 TO SHADE-COL.
	     MOVE 23		 TO SHADE-WIDTH.
	     MOVE 13		 TO SHADE-LINES.
	     DISPLAY S05.
	   IF LS0-LEV < 3
	       DISPLAY "P.D. Cheques (N/A)" AT 1030
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 3.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.
	     MOVE 7		 TO TOP-ROW.
	     MOVE 17		 TO BOTTOM-ROW.
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
	       IF (WS-OPTION < 0 OR > 8) AND
		  (WS-OPTION < "A" OR > "B")
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
	   GO TO EA18.

       EA16.
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

       EA18.
	   IF WS-OPTION = "0"
	       GO TO EA999.
	   IF WS-OPTION = "1"
	       MOVE "CRP\CRP021" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "2"
	       MOVE "CRP\CRP022" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "3"
	       MOVE "CRP\CRP023" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "4"
	       MOVE "CRP\CRP024" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "5"
	       MOVE "CRP\CRP025" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "6"
	       IF LS0-LEV < 3
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		   Not available in this version	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
		   MOVE "MNU\NOPMENU" TO PRG-NAME
	       ELSE
		   MOVE "CRP\CRP026" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "7"
	       MOVE "CRP\CRP027" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "8"
	       MOVE "CRP\CRP028" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "A"
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё			 Purchase Tracking		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	       MOVE "CRP\CRPPUR" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "B"
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё			     Enquiries			       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	       MOVE "CRP\CRPENQ" TO PRG-NAME.
	     PERFORM AA100.
	     DISPLAY MENU-INSTRUCT.
	     GO TO EA05.

       EA999.
	     EXIT.
      /
      *    ****   C R E D I T O R   /	G - L E D G E R   T R A N S
      *
       FA000	       SECTION 53.
       FA00.
	   IF LS-DSKDRV = "L"
	       MOVE "Not available on Local drive"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO FA999.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 28		 TO SHADE-COL.
	     MOVE 22		 TO SHADE-WIDTH.
	     MOVE 10		 TO SHADE-LINES.
	     DISPLAY S06.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.
	     MOVE 7		 TO TOP-ROW.
	     MOVE 14		 TO BOTTOM-ROW.
	     MOVE 6		 TO SCREEN-LIN.
	     MOVE 28		 TO SCREEN-COL.
	     MOVE 20		 TO STRING-LENGTH.
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
	       IF WS-OPTION < 0 OR > 7
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
	       MOVE "5"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 12
	       MOVE "6"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 13
	       MOVE "7"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 14
	       MOVE "0"		 TO WS-OPTION.
	   GO TO FA18.

       FA16.
	     PERFORM CLEAR-BLOCK.
	   IF WS-OPTION = "0"
	       MOVE 14		 TO SCREEN-LIN
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
	       MOVE 13		 TO SCREEN-LIN.
	     PERFORM SAVE-ATTR.
	     PERFORM MARK-BLOCK.

       FA18.
	   IF WS-OPTION = "0"
	       GO TO FA999.
	   IF WS-OPTION = "1"
	       MOVE "CRP\CRP031" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "2"
	       MOVE "CRP\CRP032" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "3"
	       MOVE "CRP\CRP033" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "4"
	       MOVE "CRP\CRP034" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "5"
	       MOVE "CRP\CRP035" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "6"
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё			     Enquiries			       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	       MOVE "CRP\CRPENQ" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "7"
	       MOVE "GLP\GLPEN2" TO PRG-NAME.
	     PERFORM AA100.
	     DISPLAY MENU-INSTRUCT.
	     GO TO FA05.

       FA999.
	     EXIT.
      /
      *    ****    C R E D I T O R   E X P O R T S
      *
       GA000	       SECTION 54.
       GA00.
	   IF LS-DSKDRV = "L"
	       MOVE "Not available on Local drive"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO GA999.
	   IF LS0-LEV < 2
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		   Not available in this version	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	       MOVE "MNU\NOPMENU"
				 TO PRG-NAME
	   ELSE
	       MOVE "CRP\CRPEXP" TO PRG-NAME.
	     PERFORM AA100.

       GA999.
	     EXIT.
      /
      *    ****    C R E D I T O R   U T I L I T I E S
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
	     MOVE 23		 TO SHADE-WIDTH.
	     MOVE 8		 TO SHADE-LINES.
	     DISPLAY S07.
	   IF LS0-LEV < 3
	       DISPLAY "(N/A)" AT 0845
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 3
	       DISPLAY ". (N/A)" AT 0943
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 3
	       DISPLAY "(N/A)" AT 1045
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 3.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.
	     MOVE 7		 TO TOP-ROW.
	     MOVE 12		 TO BOTTOM-ROW.
	     MOVE 6		 TO SCREEN-LIN.
	     MOVE 28		 TO SCREEN-COL.
	     MOVE 21		 TO STRING-LENGTH.
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
	       IF WS-OPTION < 0 OR > 5 AND
		  (WS-OPTION NOT = "@")
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
	       MOVE "0"		 TO WS-OPTION.
	   GO TO HA18.

       HA16.
	     PERFORM CLEAR-BLOCK.
	   IF WS-OPTION = 0
	       MOVE 12		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = 1
	       MOVE 7		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = 2
	       MOVE 8		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = 3
	       MOVE 9		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = 4
	       MOVE 10		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = 5
	       MOVE 11		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "@"
	       MOVE 7		 TO SCREEN-LIN.
	     PERFORM SAVE-ATTR.
	     PERFORM MARK-BLOCK.

       HA18.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF WS-OPTION = "0"
	       GO TO HA999.
	   IF WS-OPTION = "1"
	       IF LS0-LEV < 3
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		   Not available in this version	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
		   MOVE "MNU\NOPMENU" TO PRG-NAME
	       ELSE
		   MOVE "CRP\CRP091" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "2"
	       IF LS0-LEV < 3
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		   Not available in this version	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
		   MOVE "MNU\NOPMENU" TO PRG-NAME
	       ELSE
		   MOVE "CRP\CRP092" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "3"
	       IF LS0-LEV < 3
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		   Not available in this version	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
		   MOVE "MNU\NOPMENU" TO PRG-NAME
	       ELSE
		   MOVE "CRP\CRP093" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "4"
	       IF LS0-LEV < 3
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		   Not available in this version	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
		   MOVE "MNU\NOPMENU" TO PRG-NAME
	       ELSE
		   MOVE "CRP\CRP094" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "5"
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё			     Enquiries			       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	       MOVE "CRP\CRPENQ" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "@"
	       MOVE "CRP\CRPTRN" TO PRG-NAME.
	     PERFORM AA100.
	     DISPLAY MENU-INSTRUCT.
	     GO TO HA05.

       HA999.
	     EXIT.
      /
      *    ****    C R E D I T O R   E N Q U I R I E S
      *
       IA000	       SECTION 56.
       IA00.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё			     Enquiries			       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     MOVE "CRP\CRPENQ"	 TO PRG-NAME.
	     PERFORM AA100.

       IA999.
	     EXIT.
      /
      *    ****    C R E D I T O R   E N Q U I R I E S
      *
       JA000	       SECTION 56.
       JA00.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		       Purchase Tracking		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     MOVE "CRP\CRPPUR"	 TO PRG-NAME.
	     PERFORM AA100.

       JA999.
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
      *    *************************************************************
      *    ****   ( A / P )   C R E D I T O R	M E M O	  F I L E   ****
      *    *************************************************************
	     MOVE "CRDMEM"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CRDMEM.
      *    *************************************************************
      *    *** C R E D I T O R	 A C C O U N T	 ( A / P )   F I L E ***
      *    *************************************************************
	     MOVE "CREDIT"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CREDIT.
      *    *************************************************************
      *    ( A / P )   C R E D I T O R	 T R A N S A C T I O N	 F I L E
      *    *************************************************************
	     MOVE "CRTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CRTRAN.
      *    *************************************************************
      *    **** 	  I N T E G R A T I O N	  F I L E	    ****
      *    *************************************************************
	     MOVE "LEDTRF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-LEDTRF.
	     MOVE "PDCHEQ"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-PDCHEQ.
      *    *************************************************************
      *    ****        P U R C H A S E	 O R D E R   F I L E	    ****
      *    *************************************************************
	     MOVE "PORDER"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-PORDER.
      *    *************************************************************
      *    ****    P U R C H A S E S   -   C A R D E X	 F I L E    ****
      *    *************************************************************
	     MOVE "PURDEX"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-PURDEX.
      *    *************************************************************
      *    ****	S T O C K   A L T E R N A T E	I N D E X   F I L E ****
      *    *************************************************************
	     MOVE "STKALT"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STKALT.
      *    *************************************************************
      *    **** 	   S T O C K   M E M O	 F I L E	    ****
      *    *************************************************************
	     MOVE "STKMEM"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STKMEM.
      *    *************************************************************
      *    **** 	       S T O C K   F I L E		    ****
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
042060	   IF LS0-STP NOT = 2
042070         GO TO ZA05.
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
044300	     OPEN I-O STKMEM.
044310	   IF RUNTIME-ERROR
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
042080       OPEN I-O STOCK.
042090     IF RUNTIME-ERROR
               IF FLE-LOCKED
042130             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
	       IF IDX-CORRUPT
                   MOVE 22       TO WS-F-ERROR
                   GO TO ZA50.
050340	   IF WS-STATUS NOT = "00"
050350         MOVE 22           TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
050360		   PERFORM OPEN-ERROR.
042170       OPEN I-O PORDER.
042180	   IF RUNTIME-ERROR
               IF FLE-LOCKED
042130             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
	       IF IDX-CORRUPT
                   MOVE 16       TO WS-F-ERROR
                   GO TO ZA50.
050190     IF WS-STATUS NOT = "00"
050200         MOVE 16           TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
050210		   PERFORM OPEN-ERROR.
042200 ZA05.
042210	     OPEN I-O CTRANS.
042220     IF RUNTIME-ERROR
               IF FLE-LOCKED
042230             GO TO ZA200
               ELSE
               IF FLE-LIMIT
		   GO TO ZA49
	       ELSE
	       IF IDX-CORRUPT
		   MOVE 4	 TO WS-F-ERROR
                   GO TO ZA50.
050190     IF WS-STATUS NOT = "00"
050200         MOVE 4            TO WS-F-ERROR
	       IF NOT (WS-STATUS = "41")
050210		   PERFORM OPEN-ERROR.
042240       OPEN I-O CREDIT.
042250	   IF RUNTIME-ERROR
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
042240	     OPEN I-O CRDMEM.
	   IF WS-STATUS = "05"
	       CLOSE CRDMEM
	       OPEN OUTPUT CRDMEM
	       CLOSE CRDMEM
	       OPEN I-O CRDMEM.
042250	   IF RUNTIME-ERROR
               IF FLE-LOCKED
042130             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
	       IF IDX-CORRUPT
		   MOVE 14	 TO WS-F-ERROR
                   GO TO ZA50.
050190     IF WS-STATUS NOT = "00"
050200	       MOVE 14		 TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
050210		   PERFORM OPEN-ERROR.
044300	     OPEN I-O LEDTRF.
           IF WS-STATUS = "05"
               CLOSE LEDTRF
               OPEN OUTPUT LEDTRF
               CLOSE LEDTRF
               OPEN I-O LEDTRF.
044310	   IF RUNTIME-ERROR
               IF FLE-LOCKED
040420             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
	       IF IDX-CORRUPT
                   MOVE 40       TO WS-F-ERROR
                   GO TO ZA50.
050190     IF WS-STATUS NOT = "00"
050200         MOVE 40           TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
050210		   PERFORM OPEN-ERROR.
019900	     OPEN I-O PDCHEQ.
	   IF WS-STATUS = "05"
	       CLOSE PDCHEQ
	       OPEN OUTPUT PDCHEQ
	       CLOSE PDCHEQ
	       OPEN I-O PDCHEQ.
019910	   IF RUNTIME-ERROR
               IF FLE-LOCKED
042130             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
	       IF IDX-CORRUPT
		   MOVE 44	 TO WS-F-ERROR
                   GO TO ZA50.
050190	   IF WS-STATUS NOT = "00"
050200	       MOVE 44		  TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
050210		   PERFORM OPEN-ERROR.
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
053830	     OPEN I-O PURDEX.
	   IF WS-STATUS = "05"
	       CLOSE PURDEX
	       OPEN OUTPUT PURDEX
	       CLOSE PURDEX
	       OPEN I-O PURDEX.
053840	   IF RUNTIME-ERROR
053850         IF FLE-LOCKED
053860             GO TO ZA200
053870         ELSE
053880         IF FLE-LIMIT
053890		   GO TO ZA49
               ELSE
	       IF IDX-CORRUPT
		   MOVE 56	 TO WS-F-ERROR
		   GO TO ZA50.
053900     IF WS-STATUS NOT = "00"
053910	       MOVE 56		 TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
053920		   PERFORM OPEN-ERROR.
	   IF LS-DEM = 6
	       MOVE "CRP\CRP091" TO PRG-NAME
	       PERFORM AA100.
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
	   ELSE
041650	   IF WS-F-ERROR = 4
041660	       MOVE "CR. TRANSACTION"
				 TO WS-FILE
	   ELSE
	   IF WS-F-ERROR = 14
	       MOVE "CREDIT MEMO"
				 TO WS-FILE
041840	   ELSE
041850     IF WS-F-ERROR = 16
041860         MOVE "PURCHASE ORDER"
				 TO WS-FILE
041840	   ELSE
041850     IF WS-F-ERROR = 22
041860         MOVE "STOCK FILE" TO WS-FILE
           ELSE
           IF WS-F-ERROR = 25
               MOVE "STOCK MEMO" TO WS-FILE
	   ELSE
041850     IF WS-F-ERROR = 40
041860         MOVE "INTEGRATION"
				 TO WS-FILE
041840	   ELSE
041850	   IF WS-F-ERROR = 44
041860	       MOVE "PD CHEQUE File"
				 TO WS-FILE
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
				 TO WS-FILE
041840	   ELSE
041850	   IF WS-F-ERROR = 56
041860	       MOVE "CARDEX File"
				 TO WS-FILE.
             DISPLAY "Rebuild " AT 0812
                      WS-FILE WITH FOREGROUND-COLOR 14.
             DISPLAY "Press any key to continue" AT 1012.
             ACCEPT WS-OPTION AT 1038 WITH FOREGROUND-COLOR 15.
       ZA51.
             EXIT PROGRAM.
      *
      *    ****    S E T U P   T H E   S C R E E N   G R A P H I C S
      *
040090 ZA60.
040650	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "Supervisorд"  TO WS-WRKHD
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
041650	   IF WS-F-ERROR = 3
041660         MOVE W02-CREDIT   TO WS-FILE
041670         MOVE ZERO         TO WS-KEY
	       MOVE P-NUMBER	 TO WS-KEYX
041680     ELSE
041690     IF WS-F-ERROR = 4
041700         MOVE W02-CRTRAN   TO WS-FILE
041710	       MOVE ZERO	 TO WS-KEY
	       MOVE CT-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 14
041860	       MOVE W02-CRDMEM	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE CME-KEY	 TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 16
041860         MOVE W02-PORDER   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE ORD-KEY	 TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 22
041860         MOVE W02-STOCKF   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
               MOVE STK-CODE     TO WS-KEYX
041840     ELSE
041850     IF WS-F-ERROR = 25
041860         MOVE W02-STKMEM   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
               MOVE SME-KEY      TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 40
041860         MOVE W02-LEDTRF   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE XFR-KEY	 TO WS-KEYX
041840     ELSE
041850	   IF WS-F-ERROR = 44
041860	       MOVE W02-PDCHEQ	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
	       MOVE PDT-KEY	 TO WS-KEYX
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
	       MOVE WST-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 56
041860	       MOVE W02-PURDEX	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE PRD-KEY	 TO WS-KEYX.
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
