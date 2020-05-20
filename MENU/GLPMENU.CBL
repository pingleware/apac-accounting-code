      $set LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *   ******   **	     *******   **     **  ********  **	   **  *
      *  **    **  **	     **	   **  ***   ***  **	    ***	   **  *
      *  **	   **	     **	   **  ** * * **  **	    ** *   **  *
      *  **	   **	     *******   **  *  **  *****	    **	*  **  *
      *  **   ***  **	     **	       **     **  **	    **	 * **  *
      *  **    **  **	     **	       **     **  **	    **	  ***  *
      *   ******   ********  **        **     **  *******   **	   **  *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     G / L E D G E R   M O D U L E   M E N U   P R O G R A M    *
      *                                                                *
      *     VERSION 8.15.02 - November 2011			       *
      * 							       *
      ******************************************************************
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
000020 PROGRAM-ID.     GLPMENU.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   JANUARY 1983.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1983 - 2012
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

000160 COPY GACCNT.SL.

000180 COPY GLTRAN.SL.

       COPY LEDTRF.SL.

      /
000330 DATA DIVISION.
000340 FILE SECTION.

       COPY APACFIDS.FDE.

000320 COPY GACCNT.FDE.

000340 COPY GLTRAN.FDE.

       COPY LEDTRF.FDE.

      /
000560 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
000610 77  WS-S1	   PIC	9(04)	 COMP-5.
000620 77  WS-S2           PIC  9(04)    COMP-5.
000630 77  WS-S3           PIC  9(04)    COMP-5.
000640 77  WS-S4           PIC  9(04)    COMP-5.
       77  WS-TEMP-LIN	   PIC	9(02)	 COMP-5.
000770 77  WS-GLKEY	   PIC	9(04)	 COMP-5.
000820 77  WS-GINKEY	   PIC	9(04)	 COMP-5.
000750 77  WS-PARKEY       PIC  9(04)    COMP-5.
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

000320 COPY GACCNT.ID.

000340 COPY GLTRAN.ID.

       COPY LEDTRF.ID.

       COPY PARAM.ID.

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
001460				 VALUE "G/LEDGER CONTROL MODULE".

       01  S02.
	   03  LINE  4 COLUMN  5 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
	   03	       COLUMN 30 BACKGROUND-COLOR 2 FOREGROUND-COLOR 0
				 VALUE "¿".
	   03  LINE  5 COLUMN  5 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "GENERAL LEDGER PROGRAMS".
	   03	       COLUMN 30 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "³".
	   03  LINE  6 COLUMN  5 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "1".
	   03	       COLUMN  8 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Maintenance/Transact ³".
001470	   03  LINE  7 COLUMN  5 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "2".
	   03	       COLUMN  8 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Reports    	      ³".
001470	   03  LINE  8 COLUMN  5 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "3".
	   03	       COLUMN  8 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Period-End Transfers ³".
001470	   03  LINE  9 COLUMN  5 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "4".
	   03	       COLUMN  8 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Batch Transactions   ³".
001570	   03  LINE 10 COLUMN  5 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "5".
	   03	       COLUMN  8 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Standing Entries     ³".
001570	   03  LINE 11 COLUMN  5 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "6".
	   03	       COLUMN  8 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Export Options       ³".
001570	   03  LINE 12 COLUMN  5 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "7".
	   03	       COLUMN  8 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Utilities            ³".
001570	   03  LINE 13 COLUMN  5 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "8".
	   03	       COLUMN  8 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Enquiries            ³".
001570	   03  LINE 14 COLUMN  5 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "A".
	   03	       COLUMN  8 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Basic Chart of Acnts ³".
001500	   03  LINE 15 COLUMN  5 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "0".
	   03	       COLUMN  8 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Exit	              ³".
	   03  LINE 16 COLUMN  5 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "À".
	   03	       COLUMN  6 BACKGROUND-COLOR 2 FOREGROUND-COLOR 0
				 VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".


       01  S03.
	   03  LINE  6 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
	   03	       COLUMN 52 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
				 VALUE "¿".
	   03  LINE  7 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "MAINTENANCE/TRANSACTIONS".
	   03	       COLUMN 52 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE "³".
	   03  LINE  8 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "1".
	   03	       COLUMN 30 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Account Maintainance ³".
001470	   03  LINE  9 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "2".
	   03	       COLUMN 30 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Sundry Maintainance  ³".
001570	   03  LINE 10 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "3".
	   03	       COLUMN 30 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Transactions         ³".
001500	   03  LINE 11 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "4".
	   03	       COLUMN 30 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Enquiries            ³".
001500	   03  LINE 12 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "5".
	   03	       COLUMN 30 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Reconcile Accounts   ³".
	   03  LINE 13 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "0".
	   03	       COLUMN 30 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Exit	              ³".
	   03  LINE 14 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "À".
	   03	       COLUMN 28 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
				 VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S04.
	   03  LINE  8 COLUMN 49 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
	   03	       COLUMN 75 BACKGROUND-COLOR 7 FOREGROUND-COLOR 0
				 VALUE "¿".
	   03  LINE  9 COLUMN 49 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³   ".
	   03	       COLUMN 53 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "ACCOUNT MAINTENANCE".
	   03	       COLUMN 72 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE "   ³".
	   03  LINE 10 COLUMN 49 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 51 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "1".
	   03	       COLUMN 52 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Create/Amend Account  ³".
	   03  LINE 11 COLUMN 49 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 51 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "2".
	   03	       COLUMN 52 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Delete Account        ³".
	   03  LINE 12 COLUMN 49 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 51 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "3".
	   03	       COLUMN 52 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Budgets               ³".
	   03  LINE 13 COLUMN 49 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 51 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "4".
	   03	       COLUMN 52 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Previous Year Figures ³".
	   03  LINE 14 COLUMN 49 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 51 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "5".
	   03	       COLUMN 52 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Current Year Figures  ³".
	   03  LINE 15 COLUMN 49 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 51 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "6".
	   03	       COLUMN 52 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Enquiries             ³".
	   03  LINE 16 COLUMN 49 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 51 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "0".
	   03	       COLUMN 52 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Exit                  ³".
	   03  LINE 17 COLUMN 49 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "À".
	   03	       COLUMN 50 BACKGROUND-COLOR 7 FOREGROUND-COLOR 0
				 VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S05.
	   03  LINE  8 COLUMN 49 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
	   03	       COLUMN 77 BACKGROUND-COLOR 7 FOREGROUND-COLOR 0
				 VALUE "¿".
	   03  LINE  9 COLUMN 49 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³    ".
	   03	       COLUMN 54 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "SUNDRY MAINTENANCE".
	   03	       COLUMN 72 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE "     ³".
	   03  LINE 10 COLUMN 49 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 51 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "1".
	   03	       COLUMN 52 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Accumulate Group Totals ³".
	   03  LINE 11 COLUMN 49 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 51 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "2".
	   03	       COLUMN 52 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Accumulate Previous Year³".
	   03  LINE 12 COLUMN 49 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 51 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "3".
	   03	       COLUMN 52 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Month End Routine       ³".
	   03  LINE 13 COLUMN 49 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 51 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "4".
	   03	       COLUMN 52 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Year End Routine        ³".
	   03  LINE 14 COLUMN 49 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 51 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "5".
	   03	       COLUMN 52 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Set Year End Month      ³".
	   03  LINE 15 COLUMN 49 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 51 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "6".
	   03	       COLUMN 52 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Enquiries               ³".
	   03  LINE 16 COLUMN 49 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 51 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "0".
	   03	       COLUMN 52 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Exit                    ³".
	   03  LINE 17 COLUMN 49 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "À".
	   03	       COLUMN 50 BACKGROUND-COLOR 7 FOREGROUND-COLOR 0
				 VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S06.
	   03  LINE  8 COLUMN 49 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
	   03	       COLUMN 77 BACKGROUND-COLOR 7 FOREGROUND-COLOR 0
				 VALUE "¿".
	   03  LINE  9 COLUMN 49 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³    ".
	   03	       COLUMN 54 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "TRANSACTION OPTIONS".
	   03	       COLUMN 73 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE "    ³".
	   03  LINE 10 COLUMN 49 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 51 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "1".
	   03	       COLUMN 52 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Cheque Requisitions     ³".
	   03  LINE 11 COLUMN 49 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 51 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "2".
	   03	       COLUMN 52 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Bank Deposits           ³".
	   03  LINE 12 COLUMN 49 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 51 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "3".
	   03	       COLUMN 52 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Bank Charges/Receipts   ³".
	   03  LINE 13 COLUMN 49 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 51 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "4".
	   03	       COLUMN 52 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Journal Entries         ³".
	   03  LINE 14 COLUMN 49 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 51 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "5".
	   03	       COLUMN 52 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Electronic Fund Transfer³".
	   03  LINE 15 COLUMN 49 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 51 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "6".
	   03	       COLUMN 52 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Petty Cash Payments     ³".
	   03  LINE 16 COLUMN 49 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 51 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "7".
	   03	       COLUMN 52 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Petty Cash Receipts     ³".
	   03  LINE 17 COLUMN 49 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 51 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "8".
	   03	       COLUMN 52 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Journal Entries Single  ³".
	   03  LINE 18 COLUMN 49 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 51 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "A".
	   03	       COLUMN 52 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Enquiries               ³".
	   03  LINE 19 COLUMN 49 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 51 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "0".
	   03	       COLUMN 52 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Exit                    ³".
	   03  LINE 20 COLUMN 49 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "À".
	   03	       COLUMN 50 BACKGROUND-COLOR 7 FOREGROUND-COLOR 0
				 VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S06A.
	   03  LINE  8 COLUMN 49 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
	   03	       COLUMN 77 BACKGROUND-COLOR 7 FOREGROUND-COLOR 0
				 VALUE "¿".
	   03  LINE  9 COLUMN 49 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³     ".
	   03	       COLUMN 55 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "RECONCILE OPTIONS".
	   03	       COLUMN 72 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE "     ³".
	   03  LINE 10 COLUMN 49 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 51 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "1".
	   03	       COLUMN 52 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Reconcile by Value      ³".
	   03  LINE 11 COLUMN 49 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 51 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "2".
	   03	       COLUMN 52 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Reconcile Cheques       ³".
	   03  LINE 12 COLUMN 49 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 51 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "3".
	   03	       COLUMN 52 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Reconcile Elect. Funds  ³".
	   03  LINE 13 COLUMN 49 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 51 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "4".
	   03	       COLUMN 52 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Reconcile Transactions  ³".
	   03  LINE 14 COLUMN 49 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 51 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "5".
	   03	       COLUMN 52 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Reconciliation Statement³".
	   03  LINE 15 COLUMN 49 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 51 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "6".
	   03	       COLUMN 52 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Cheques in No. sequence ³".
	   03  LINE 16 COLUMN 49 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 51 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "7".
	   03	       COLUMN 52 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Enquiries               ³".
	   03  LINE 17 COLUMN 49 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 51 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "0".
	   03	       COLUMN 52 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Exit                    ³".
	   03  LINE 18 COLUMN 49 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "À".
	   03	       COLUMN 50 BACKGROUND-COLOR 7 FOREGROUND-COLOR 0
				 VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S07.
	   03  LINE  6 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
	   03	       COLUMN 52 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
				 VALUE "¿".
	   03  LINE  7 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³    ".
	   03	       COLUMN 32 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "G/LEDGER REPORTS".
	   03	       COLUMN 48 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE "    ³".
	   03  LINE  8 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "1".
	   03	       COLUMN 30 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Chart of Accounts    ³".
001470	   03  LINE  9 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "2".
	   03	       COLUMN 30 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Trial Balance        ³".
001570	   03  LINE 10 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "3".
	   03	       COLUMN 30 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Operating Statement  ³".
001500	   03  LINE 11 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "4".
	   03	       COLUMN 30 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Detailed Account List³".
	   03  LINE 12 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "5".
	   03	       COLUMN 30 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Balance Sheet        ³".
	   03  LINE 13 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "6".
	   03	       COLUMN 30 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Cost Centre P & L    ³".
	   03  LINE 14 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "7".
	   03	       COLUMN 30 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Sub/Group Schedules  ³".
	   03  LINE 15 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "0".
	   03	       COLUMN 30 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Exit	              ³".
	   03  LINE 16 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "À".
	   03	       COLUMN 28 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
				 VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S08.
	   03  LINE  6 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
	   03	       COLUMN 52 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
				 VALUE "¿".
	   03  LINE  7 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³   ".
	   03	       COLUMN 31 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "RECURRING ENTRIES".
	   03	       COLUMN 48 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE "    ³".
	   03  LINE  8 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "1".
	   03	       COLUMN 30 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Create Standing Entry³".
001470	   03  LINE  9 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "2".
	   03	       COLUMN 30 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Delete Standing Entry³".
001570	   03  LINE 10 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "3".
	   03	       COLUMN 30 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Amend Standing Entry ³".
001500	   03  LINE 11 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "4".
	   03	       COLUMN 30 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Post Standing Entries³".
001500	   03  LINE 12 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "5".
	   03	       COLUMN 30 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " List Standing Entries³".
	   03  LINE 13 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "0".
	   03	       COLUMN 30 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Exit	              ³".
	   03  LINE 14 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "À".
	   03	       COLUMN 28 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
				 VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S09.
	   03  LINE  6 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
	   03	       COLUMN 52 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
				 VALUE "¿".
	   03  LINE  7 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³  ".
	   03	       COLUMN 30 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "PERIOD-END ROUTINES".
	   03	       COLUMN 49 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE "   ³".
	   03  LINE  8 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "1".
	   03	       COLUMN 30 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Review Integrat. Btch³".
001470	   03  LINE  9 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "2".
	   03	       COLUMN 30 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Print Integrat. Batch³".
001570	   03  LINE 10 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "3".
	   03	       COLUMN 30 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Post Integrat. Batch ³".
001500	   03  LINE 11 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "4".
	   03	       COLUMN 30 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Amend Integrat Batch ³".
001500	   03  LINE 12 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "5".
	   03	       COLUMN 30 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Year-end Transfers   ³".
001500	   03  LINE 13 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "0".
	   03	       COLUMN 30 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Exit	              ³".
	   03  LINE 14 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "À".
	   03	       COLUMN 28 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
				 VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S10.
	   03  LINE  6 COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
	   03	       COLUMN 50 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
				 VALUE "¿".
	   03  LINE  7 COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³  ".
	   03	       COLUMN 32 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "BATCH PROCESSING".
	   03	       COLUMN 48 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE "  ³".
	   03  LINE  8 COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "1".
	   03	       COLUMN 32 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Journal Entries  ³".
001470	   03  LINE  9 COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "2".
	   03	       COLUMN 32 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Cheques Issued   ³".
001570	   03  LINE 10 COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "3".
	   03	       COLUMN 32 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Deposits         ³".
001500	   03  LINE 11 COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "4".
	   03	       COLUMN 32 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Enquiries        ³".
001500	   03  LINE 12 COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "0".
	   03	       COLUMN 32 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Exit	          ³".
	   03  LINE 13 COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "À".
	   03	       COLUMN 30 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
				 VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S11.
	   03  LINE  8 COLUMN 49 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
	   03	       COLUMN 74 BACKGROUND-COLOR 7 FOREGROUND-COLOR 0
				 VALUE "¿".
	   03  LINE  9 COLUMN 49 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³    ".
	   03	       COLUMN 54 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "JOURNAL -BATCHES".
	   03	       COLUMN 70 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE "    ³".
	   03  LINE 10 COLUMN 49 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 51 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "1".
	   03	       COLUMN 52 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Create/Add to a Batch³".
001470	   03  LINE 11 COLUMN 49 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 51 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "2".
	   03	       COLUMN 52 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Review/Amend a Batch ³".
001570	   03  LINE 12 COLUMN 49 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 51 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "3".
	   03	       COLUMN 52 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Print Existing Batch ³".
001500	   03  LINE 13 COLUMN 49 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 51 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "4".
	   03	       COLUMN 52 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Delete Existing Batch³".
001500	   03  LINE 14 COLUMN 49 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 51 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "5".
	   03	       COLUMN 52 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Process a Batch      ³".
	   03  LINE 15 COLUMN 49 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 51 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "0".
	   03	       COLUMN 52 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Exit	              ³".
	   03  LINE 16 COLUMN 49 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "À".
	   03	       COLUMN 50 BACKGROUND-COLOR 7 FOREGROUND-COLOR 0
				 VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S12.
	   03  LINE  6 COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
	   03	       COLUMN 50 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
				 VALUE "¿".
	   03  LINE  7 COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³   ".
	   03	       COLUMN 33 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "EXPORT OPTIONS".
	   03	       COLUMN 47 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE "   ³".
	   03  LINE  8 COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "1".
	   03	       COLUMN 32 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Account Deatails ³".
001470	   03  LINE  9 COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "2".
	   03	       COLUMN 32 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Transactions     ³".
001570	   03  LINE 10 COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "0".
	   03	       COLUMN 32 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Exit	          ³".
	   03  LINE 11 COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "À".
	   03	       COLUMN 30 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
				 VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S90.
	   03  LINE  6 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
	   03	       COLUMN 52 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
				 VALUE "¿".
	   03  LINE  7 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³    ".
	   03	       COLUMN 32 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "UTILITY PROGRAMS".
	   03	       COLUMN 48 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE "    ³".
	   03  LINE  8 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "1".
	   03	       COLUMN 30 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Clear All Balances   ³".
001470	   03  LINE  9 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "2".
	   03	       COLUMN 30 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Recalculate Balances ³".
001570	   03  LINE 10 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "3".
	   03	       COLUMN 30 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Balancing Utilities  ³".
001500	   03  LINE 11 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "4".
	   03	       COLUMN 30 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Enquiries            ³".
001500	   03  LINE 12 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "5".
	   03	       COLUMN 30 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Amend Transact Date  ³".
001500	   03  LINE 13 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "6".
	   03	       COLUMN 30 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Delete a Tran Batch  ³".
001500*	   03  LINE 14 COLUMN 27 BACKGROUND-COLOR 3
      *				 FOREGROUND-COLOR 0
      *				 VALUE "³ ".
      *	   03	       COLUMN 29 BACKGROUND-COLOR 3
      *				 FOREGROUND-COLOR 7
      *				 HIGHLIGHT
      *				 VALUE "7".
      *	   03	       COLUMN 30 BACKGROUND-COLOR 3
      *				 FOREGROUND-COLOR 0
      *				 VALUE " Change Account Number³".
001500	   03  LINE 14 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "0".
	   03	       COLUMN 30 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Exit	              ³".
	   03  LINE 15 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "À".
	   03	       COLUMN 28 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
				 VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       COPY ERROR.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
       AA000	       SECTION.
       AA00.
	     PERFORM ZA000-INIT.
	     DISPLAY CLR-L1-2.
	     DISPLAY S01.
003240	   IF LS0-GLP NOT = 2
	       MOVE "Not Installed"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	   ELSE
003240	   IF LS0-GL = "N"
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
	     CLOSE GACCNT
		   GLTRAN
		   LEDTRF.

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

       COPY LOCKED.RC2.

011433 COPY CLEAR.CRT.

       BA000	       SECTION 50.
       BA00.
	     MOVE 5		 TO SHADE-ROW.
	     MOVE 7		 TO SHADE-COL.
	     MOVE 24		 TO SHADE-WIDTH.
	     MOVE 12		 TO SHADE-LINES.
	     DISPLAY S02.
	   IF LS0-LEV < 2
	       DISPLAY ". (N/A)" AT 0923
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
	       DISPLAY "(N/A)" AT 1025
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
	       DISPLAY "(N/A)" AT 1124
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.
       BA02.
	     MOVE 5		 TO TOP-ROW.
	     MOVE 14		 TO BOTTOM-ROW.
	     MOVE 4		 TO SCREEN-LIN.
	     MOVE 7		 TO SCREEN-COL.
	     MOVE 22		 TO STRING-LENGTH.
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
		  (WS-OPTION NOT = "A")
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
	       MOVE "0"		 TO WS-OPTION.
	   GO TO BA18.

       BA16.
	     PERFORM CLEAR-BLOCK.
	   IF WS-OPTION = "0"
	       MOVE 14		 TO SCREEN-LIN
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
	       MOVE 13		 TO SCREEN-LIN.
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
	       PERFORM FB000
*	       PERFORM FA000
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
	       PERFORM KA000.
	     PERFORM RESTORE-SCREEN.
	     MOVE BLOCK-DETAIL1  TO BLOCK-DETAIL.
	     MOVE SHADOW-DETAIL1 TO SHADOW-DETAIL.
	     MOVE CRT-DETAIL1	 TO CRT-DETAIL.
	     GO TO BA05.
       BA999.
	     EXIT.
      /
      *    ****    L E D G E R	 M A I N T E N A N C E
      *
       CA000	       SECTION 50.
       CA00.
	   IF LS-DSKDRV = "L"
	       MOVE "Not available on Local drive"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO CA999.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 29		 TO SHADE-COL.
	     MOVE 24		 TO SHADE-WIDTH.
	     MOVE 8		 TO SHADE-LINES.
	     DISPLAY S03.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.
	     MOVE 7		 TO TOP-ROW.
	     MOVE 12		 TO BOTTOM-ROW.
	     MOVE 6		 TO SCREEN-LIN.
	     MOVE 29		 TO SCREEN-COL.
	     MOVE 22		 TO STRING-LENGTH.
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
	       IF WS-OPTION < 0 OR > 5
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
	       MOVE "0"		 TO WS-OPTION.
	   GO TO CA18.

       CA16.
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
	       MOVE 11		 TO SCREEN-LIN.
	     PERFORM SAVE-ATTR.
	     PERFORM MARK-BLOCK.

       CA18.
	   IF WS-OPTION = "0"
	       GO TO CA999.
	     PERFORM SAVE-SCREEN-2.
	     MOVE BLOCK-DETAIL	 TO BLOCK-DETAIL2.
	     MOVE SHADOW-DETAIL
				 TO SHADOW-DETAIL2.
	     MOVE CRT-DETAIL	 TO CRT-DETAIL2.
	   IF WS-OPTION = "1"
	       PERFORM CB000
	   ELSE
	   IF WS-OPTION = "2"
	       PERFORM CC000
	   ELSE
	   IF WS-OPTION = "3"
	       PERFORM CD000
	   ELSE
	   IF WS-OPTION = "4"
	       PERFORM CE000
	   ELSE
	   IF WS-OPTION = "5"
	       PERFORM CF000.
	     PERFORM RESTORE-SCREEN-2.
	     MOVE BLOCK-DETAIL2  TO BLOCK-DETAIL.
	     MOVE SHADOW-DETAIL2 TO SHADOW-DETAIL.
	     MOVE CRT-DETAIL2	 TO CRT-DETAIL.
	     GO TO CA05.

       CA999.
	     EXIT.

       CB000	       SECTION 50.
       CB00.
	     MOVE 9		 TO SHADE-ROW.
	     MOVE 51		 TO SHADE-COL.
	     MOVE 25		 TO SHADE-WIDTH.
	     MOVE 9		 TO SHADE-LINES.
	     DISPLAY S04.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.
	     MOVE 9		 TO TOP-ROW.
	     MOVE 15		 TO BOTTOM-ROW.
	     MOVE 8		 TO SCREEN-LIN.
	     MOVE 51		 TO SCREEN-COL.
	     MOVE 22		 TO STRING-LENGTH.
	     PERFORM SAVE-ATTR.

       CB05.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       CB10.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY	 PERFORM MOVE-BLOCK-UP
		 WHEN DOWN-KEY	 PERFORM MOVE-BLOCK-DOWN
		 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
				 IF MOUSE-ENTER = "Y"
				     MOVE "N"	 TO MOUSE-ENTER
				     GO TO CB15
				 END-IF
				 IF MOUSE-Y = 24
				     IF MOUSE-X = 5
					 MOVE 5  TO KEY-CODE-1
					 GO TO CB10
				     ELSE
				     IF MOUSE-X = 10
					 MOVE 6  TO KEY-CODE-1
					 GO TO CB10
				     ELSE
				     IF MOUSE-X > 36 AND < 44
					 MOVE 0  TO KEY-CODE-1
					 GO TO CB10
				     ELSE
				     IF MOUSE-X > 54 AND < 60
					 MOVE 0  TO KEY-CODE-1
					 MOVE 1  TO KEY-TYPE
					 GO TO CB10
				     END-IF
				 END-IF
		 WHEN ENTER-KEY  GO TO CB15
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CB05
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE "0"	 TO WS-OPTION
		     GO TO CB18
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CB05
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	       IF WS-OPTION < 0 OR > 6
		   CALL X"E5"
		   GO TO CB05
	       END-IF
	       GO TO CB16
	   ELSE
	       CALL X"E5"
	       GO TO CB05.

       CB15.
	   IF SCREEN-LIN < 9
	       CALL X"E5"
	       GO TO CB05
	   ELSE
	   IF SCREEN-LIN = 9
	       MOVE "1"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 10
	       MOVE "2"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 11
	       MOVE "3"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 12
	       MOVE "4"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 13
	       MOVE "5"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 14
	       MOVE "6"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 15
	       MOVE "0"		 TO WS-OPTION.
	   GO TO CB18.

       CB16.
	     PERFORM CLEAR-BLOCK.
	   IF WS-OPTION = 0
	       MOVE 15		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = 1
	       MOVE 9		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = 2
	       MOVE 10		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = 3
	       MOVE 11		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = 4
	       MOVE 12		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = 5
	       MOVE 13		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = 6
	       MOVE 14		 TO SCREEN-LIN.
	     PERFORM SAVE-ATTR.
	     PERFORM MARK-BLOCK.

       CB18.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF WS-OPTION = "0"
	       GO TO CB999.
	   IF WS-OPTION < 1 OR > 6
	       CALL X"E5"
	       GO TO CB05.
	   IF WS-OPTION = "1"
	       MOVE "GLP\GLP001" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "2"
	       MOVE "GLP\GLP002" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "3"
	       MOVE "GLP\GLP003" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "4"
	       MOVE "GLP\GLP009" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "5"
	       MOVE "GLP\GLP010" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "6"
	       MOVE "GLP\GLPENQ" TO PRG-NAME.
	     PERFORM AA100.
	     DISPLAY MENU-INSTRUCT.
	     GO TO CB05.

       CB999.
	     EXIT.

       CC000	       SECTION 50.
       CC00.
	     MOVE 9		 TO SHADE-ROW.
	     MOVE 51		 TO SHADE-COL.
	     MOVE 27		 TO SHADE-WIDTH.
	     MOVE 9		 TO SHADE-LINES.
	     DISPLAY S05.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.
	     MOVE 9		 TO TOP-ROW.
	     MOVE 15		 TO BOTTOM-ROW.
	     MOVE 8		 TO SCREEN-LIN.
	     MOVE 51		 TO SCREEN-COL.
	     MOVE 25		 TO STRING-LENGTH.
	     PERFORM SAVE-ATTR.

       CC05.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       CC10.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY	 PERFORM MOVE-BLOCK-UP
		 WHEN DOWN-KEY	 PERFORM MOVE-BLOCK-DOWN
		 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
				 IF MOUSE-ENTER = "Y"
				     MOVE "N"	 TO MOUSE-ENTER
				     GO TO CC15
				 END-IF
				 IF MOUSE-Y = 24
				     IF MOUSE-X = 5
					 MOVE 5  TO KEY-CODE-1
					 GO TO CC10
				     ELSE
				     IF MOUSE-X = 10
					 MOVE 6  TO KEY-CODE-1
					 GO TO CC10
				     ELSE
				     IF MOUSE-X > 36 AND < 44
					 MOVE 0  TO KEY-CODE-1
					 GO TO CC10
				     ELSE
				     IF MOUSE-X > 54 AND < 60
					 MOVE 0  TO KEY-CODE-1
					 MOVE 1  TO KEY-TYPE
					 GO TO CC10
				     END-IF
				 END-IF
		 WHEN ENTER-KEY  GO TO CC15
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CC05
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE "0"	 TO WS-OPTION
		     GO TO CC18
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CC05
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	       IF WS-OPTION < 0 OR > 6 AND
		  WS-OPTION NOT = "@"
		   CALL X"E5"
		   GO TO CC05
	       END-IF
	       GO TO CC16
	   ELSE
	       CALL X"E5"
	       GO TO CC05.

       CC15.
	   IF SCREEN-LIN < 9
	       CALL X"E5"
	       GO TO CC05
	   ELSE
	   IF SCREEN-LIN = 9
	       MOVE "1"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 10
	       MOVE "2"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 11
	       MOVE "3"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 12
	       MOVE "4"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 13
	       MOVE "5"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 14
	       MOVE "6"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 15
	       MOVE "0"		 TO WS-OPTION.
	   GO TO CC18.

       CC16.
	     PERFORM CLEAR-BLOCK.
	   IF WS-OPTION = 0
	       MOVE 15		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = 1
	       MOVE 9		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = 2
	       MOVE 10		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = 3
	       MOVE 11		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = 4 OR "@"
	       MOVE 12		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = 5
	       MOVE 13		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = 6
	       MOVE 14		 TO SCREEN-LIN.
	     PERFORM SAVE-ATTR.
	     PERFORM MARK-BLOCK.

       CC18.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF WS-OPTION = "0"
	       GO TO CC999.
	   IF WS-OPTION < 1 OR > 6 AND
	      WS-OPTION NOT = "@"
	       CALL X"E5"
	       GO TO CC05.
	   IF WS-OPTION = "@"
	       MOVE "O" 	 TO LS-OK
	   ELSE
	       MOVE "N" 	 TO LS-OK.
	   IF WS-OPTION = "1"
	       MOVE "GLP\GLP004" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "2"
	       MOVE "GLP\GLP005" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "3"
	       MOVE "GLP\GLP006" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "4" OR "@"
	       MOVE "GLP\GLP007" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "5"
	       MOVE "GLP\GLP008" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "6"
	       MOVE "GLP\GLPENQ" TO PRG-NAME.
	     PERFORM AA100.
	     DISPLAY MENU-INSTRUCT.
	     GO TO CC05.

       CC999.
	     EXIT.
      /
      *    ****    L E D G E R	 T R A N S A C T I O N S
      *
       CD000	       SECTION 50.
       CD00.
	     MOVE 9		 TO SHADE-ROW.
	     MOVE 51		 TO SHADE-COL.
	     MOVE 27		 TO SHADE-WIDTH.
	     MOVE 12		 TO SHADE-LINES.
	     DISPLAY S06.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.
	     MOVE 9		 TO TOP-ROW.
	     MOVE 18		 TO BOTTOM-ROW.
	     MOVE 8		 TO SCREEN-LIN.
	     MOVE 51		 TO SCREEN-COL.
	     MOVE 25		 TO STRING-LENGTH.
	     PERFORM SAVE-ATTR.

       CD05.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       CD10.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY	 PERFORM MOVE-BLOCK-UP
		 WHEN DOWN-KEY	 PERFORM MOVE-BLOCK-DOWN
		 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
				 IF MOUSE-ENTER = "Y"
				     MOVE "N"	 TO MOUSE-ENTER
				     GO TO CD15
				 END-IF
				 IF MOUSE-Y = 24
				     IF MOUSE-X = 5
					 MOVE 5  TO KEY-CODE-1
					 GO TO CD10
				     ELSE
				     IF MOUSE-X = 10
					 MOVE 6  TO KEY-CODE-1
					 GO TO CD10
				     ELSE
				     IF MOUSE-X > 36 AND < 44
					 MOVE 0  TO KEY-CODE-1
					 GO TO CD10
				     ELSE
				     IF MOUSE-X > 54 AND < 60
					 MOVE 0  TO KEY-CODE-1
					 MOVE 1  TO KEY-TYPE
					 GO TO CD10
				     END-IF
				 END-IF
		 WHEN ENTER-KEY  GO TO CD15
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CD05
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE "0"	 TO WS-OPTION
		     GO TO CD18
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CD05
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
	       IF WS-OPTION < 0 OR > 8 AND
		  (WS-OPTION NOT = "A")
		   CALL X"E5"
		   GO TO CD05
	       END-IF
	       GO TO CD16
	   ELSE
	       CALL X"E5"
	       GO TO CD05.

       CD15.
	   IF SCREEN-LIN < 9
	       CALL X"E5"
	       GO TO CD05
	   ELSE
	   IF SCREEN-LIN = 9
	       MOVE "1"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 10
	       MOVE "2"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 11
	       MOVE "3"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 12
	       MOVE "4"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 13
	       MOVE "5"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 14
	       MOVE "6"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 15
	       MOVE "7"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 16
	       MOVE "8"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 17
	       MOVE "A"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 18
	       MOVE "0"		 TO WS-OPTION.
	   GO TO CD18.

       CD16.
	     PERFORM CLEAR-BLOCK.
	   IF WS-OPTION = "0"
	       MOVE 18		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "1"
	       MOVE 9		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "2"
	       MOVE 10		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "3"
	       MOVE 11		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "4"
	       MOVE 12		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "5"
	       MOVE 13		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "6"
	       MOVE 14		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "7"
	       MOVE 15		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "8"
	       MOVE 16		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "A"
	       MOVE 17		 TO SCREEN-LIN.
	     PERFORM SAVE-ATTR.
	     PERFORM MARK-BLOCK.

       CD18.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF WS-OPTION = "0"
	       GO TO CD999.
	   IF WS-OPTION = "1"
	       MOVE "GLP\GLP011" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "2"
	       MOVE "GLP\GLP012" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "3"
	       MOVE "GLP\GLP020" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "4"
	       MOVE "GLP\GLPJNL" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "5"
	       MOVE "GLP\GLPEFT" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "6"
	       MOVE "GLP\GLPPMT" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "7"
	       MOVE "GLP\GLPRCT" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "8"
	       MOVE "GLP\GLP013" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "A"
	       MOVE "GLP\GLPENQ" TO PRG-NAME.
	     PERFORM AA100.
	     DISPLAY MENU-INSTRUCT.
	     GO TO CD05.

       CD999.
	     EXIT.
      /
      *    ****    L E D G E R	 E N Q U I R Y
      *
       CE000	       SECTION 50.
       CE00.
	     MOVE "GLP\GLPENQ"	 TO PRG-NAME.
	     PERFORM AA100.

       CE999.
	     EXIT.
      /
      *    ****    L E D G E R	 R E C O N C I L I A T I O N S
      *
       CF000	       SECTION 50.
       CF00.
	     MOVE 9		 TO SHADE-ROW.
	     MOVE 51		 TO SHADE-COL.
	     MOVE 27		 TO SHADE-WIDTH.
	     MOVE 10		 TO SHADE-LINES.
	     DISPLAY S06A.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.
	     MOVE 9		 TO TOP-ROW.
	     MOVE 16		 TO BOTTOM-ROW.
	     MOVE 8		 TO SCREEN-LIN.
	     MOVE 51		 TO SCREEN-COL.
	     MOVE 25		 TO STRING-LENGTH.
	     PERFORM SAVE-ATTR.

       CF05.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       CF10.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY	 PERFORM MOVE-BLOCK-UP
		 WHEN DOWN-KEY	 PERFORM MOVE-BLOCK-DOWN
		 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
				 IF MOUSE-ENTER = "Y"
				     MOVE "N"	 TO MOUSE-ENTER
				     GO TO CF15
				 END-IF
				 IF MOUSE-Y = 24
				     IF MOUSE-X = 5
					 MOVE 5  TO KEY-CODE-1
					 GO TO CF10
				     ELSE
				     IF MOUSE-X = 10
					 MOVE 6  TO KEY-CODE-1
					 GO TO CF10
				     ELSE
				     IF MOUSE-X > 36 AND < 44
					 MOVE 0  TO KEY-CODE-1
					 GO TO CF10
				     ELSE
				     IF MOUSE-X > 54 AND < 60
					 MOVE 0  TO KEY-CODE-1
					 MOVE 1  TO KEY-TYPE
					 GO TO CF10
				     END-IF
				 END-IF
		 WHEN ENTER-KEY  GO TO CF15
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CF05
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE "0"	 TO WS-OPTION
		     GO TO CF18
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CF05
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
	       IF WS-OPTION < 0 OR > 7 AND
		  WS-OPTION NOT = ">"
		   CALL X"E5"
		   GO TO CF05
	       END-IF
	       GO TO CF16
	   ELSE
	       CALL X"E5"
	       GO TO CF05.

       CF15.
	   IF SCREEN-LIN < 9
	       CALL X"E5"
	       GO TO CF05
	   ELSE
	   IF SCREEN-LIN = 9
	       MOVE "1"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 10
	       MOVE "2"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 11
	       MOVE "3"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 12
	       MOVE "4"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 13
	       MOVE "5"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 14
	       MOVE "6"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 15
	       MOVE "7"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 16
	       MOVE "0"		 TO WS-OPTION.
	   GO TO CF18.

       CF16.
	     PERFORM CLEAR-BLOCK.
	   IF WS-OPTION = "0"
	       MOVE 16		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "1"
	       MOVE 9		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "2"
	       MOVE 10		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "3"
	       MOVE 11		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "4"
	       MOVE 12		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "5"
	       MOVE 13		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "6"
	       MOVE 14		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "7"
	       MOVE 15		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = ">"
	       MOVE 15		 TO SCREEN-LIN.
	     PERFORM SAVE-ATTR.
	     PERFORM MARK-BLOCK.

       CF18.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF WS-OPTION = "0"
	       GO TO CF999.
	   IF WS-OPTION = "1"
	       MOVE "GLP\GLP014" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "2"
	       MOVE "GLP\GLP015" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "3"
	       MOVE "GLP\GLP015E" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "4"
	       MOVE "GLP\GLP016" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "5"
	       MOVE "GLP\GLP017" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "6"
	       MOVE "GLP\GLP019" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "7"
	       MOVE "GLP\GLPENQ" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = ">"
	       MOVE "GLP\GLPRECON" TO PRG-NAME.
	     PERFORM AA100.
	     DISPLAY MENU-INSTRUCT.
	     GO TO CF05.

       CF999.
	     EXIT.
      /
      *    ****    L E D G E R	 R E P O R T S
      *
       DA000	       SECTION 51.
       DA00.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 29		 TO SHADE-COL.
	     MOVE 24		 TO SHADE-WIDTH.
	     MOVE 10		 TO SHADE-LINES.
	     DISPLAY S07.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.
	     MOVE 7		 TO TOP-ROW.
	     MOVE 14		 TO BOTTOM-ROW.
	     MOVE 6		 TO SCREEN-LIN.
	     MOVE 29		 TO SCREEN-COL.
	     MOVE 22		 TO STRING-LENGTH.
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
	       IF WS-OPTION < 0 OR > 7
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
	       MOVE "0"		 TO WS-OPTION.
	   GO TO DA18.

       DA16.
	     PERFORM CLEAR-BLOCK.
	   IF WS-OPTION = 0
	       MOVE 14		 TO SCREEN-LIN
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
	   IF WS-OPTION = 6
	       MOVE 12		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = 7
	       MOVE 13		 TO SCREEN-LIN.
	     PERFORM SAVE-ATTR.
	     PERFORM MARK-BLOCK.

       DA18.
	   IF WS-OPTION = "0"
	       GO TO DA999.
	   IF WS-OPTION < 1 OR > 7
	       CALL X"E5"
	       GO TO DA05.
	   IF WS-OPTION = "1"
	       MOVE "GLP\GLP021" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "2"
	       MOVE "GLP\GLP022" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "3"
	       MOVE "GLP\GLP023" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "4"
	       MOVE "GLP\GLP024" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "5"
	       MOVE "GLP\GLP025" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "6"
	       MOVE "GLP\GLP026" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "7"
	       MOVE "GLP\GLP027" TO PRG-NAME.
	     PERFORM AA100.
	     DISPLAY MENU-INSTRUCT.
	     GO TO DA05.
       DA999.
	     EXIT.
      /
      *    ****   P E R I O D - E N D	R O U T I N E S
      *
       EA000	       SECTION 52.
       EA00.
	   IF LS-DSKDRV = "L"
	       MOVE "Not available on Local drive"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO EA999.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 29		 TO SHADE-COL.
	     MOVE 24		 TO SHADE-WIDTH.
	     MOVE 8		 TO SHADE-LINES.
	     DISPLAY S09.
	   IF LS0-LEV < 2
	       DISPLAY "(N/A)" AT 0847
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 3
	       DISPLAY "(N/A)" AT 0947
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 3
	   IF LS0-LEV < 3
	       DISPLAY "(N/A)" AT 1146
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 3.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.
	     MOVE 7		 TO TOP-ROW.
	     MOVE 12		 TO BOTTOM-ROW.
	     MOVE 6		 TO SCREEN-LIN.
	     MOVE 29		 TO SCREEN-COL.
	     MOVE 22		 TO STRING-LENGTH.
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
	       IF WS-OPTION < 0 OR > 5
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
	       MOVE "0"		 TO WS-OPTION.
	   GO TO EA18.

       EA16.
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
	       MOVE 11		 TO SCREEN-LIN.
	     PERFORM SAVE-ATTR.
	     PERFORM MARK-BLOCK.

       EA18.
	   IF WS-OPTION = "0"
	       GO TO EA999.
	   IF WS-OPTION < 1 OR > 5
	       CALL X"E5"
	       GO TO EA05.
	   IF WS-OPTION = "1"
	       IF LS0-LEV < 2
		   MOVE "MNU\NOPMENU"
				 TO PRG-NAME
	       ELSE
		   MOVE "GLP\GLP051"
				 TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "2"
	       IF LS0-LEV < 2
		   MOVE "MNU\NOPMENU"
				 TO PRG-NAME
	       ELSE
		   MOVE "GLP\GLP052"
				 TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "3"
	       MOVE "GLP\GLP053" TO PRG-NAME
013040	       CLOSE LEDTRF
	   ELSE
	   IF WS-OPTION = "4"
	       IF LS0-LEV < 3
		   MOVE "MNU\NOPMENU"
				 TO PRG-NAME
	       ELSE
		   MOVE "GLP\GLP054"
				 TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "5"
	       MOVE "GLP\GLP055" TO PRG-NAME.
	     PERFORM AA100.
	   IF PRG-NAME = "GLP\GLP053"
044300	       OPEN I-O LEDTRF.
	     DISPLAY MENU-INSTRUCT.
	     GO TO EA05.

       EA999.
	     EXIT.
      /
      *    ****   B A T C H   T R A N S A C T I O N S
      *
       FA000	       SECTION 53.
       FA00.
	   IF LS0-LEV < 2
	       MOVE "MNU\NOPMENU"
				 TO PRG-NAME
	       PERFORM AA100
	       DISPLAY MENU-INSTRUCT
	       GO TO FA999.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 31		 TO SHADE-COL.
	     MOVE 20		 TO SHADE-WIDTH.
	     MOVE 7		 TO SHADE-LINES.
	     DISPLAY S10.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.
	     MOVE 7		 TO TOP-ROW.
	     MOVE 11		 TO BOTTOM-ROW.
	     MOVE 6		 TO SCREEN-LIN.
	     MOVE 31		 TO SCREEN-COL.
	     MOVE 18		 TO STRING-LENGTH.
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
	       IF WS-OPTION < 0 OR > 5
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
	   IF SCREEN-LIN = 12
	       MOVE "0"		 TO WS-OPTION.
	   GO TO FA18.

       FA16.
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
	       MOVE 10		 TO SCREEN-LIN.
	     PERFORM SAVE-ATTR.
	     PERFORM MARK-BLOCK.

       FA18.
	   IF WS-OPTION = "0"
	       GO TO FA999.
	     PERFORM SAVE-SCREEN-2.
	     MOVE BLOCK-DETAIL	 TO BLOCK-DETAIL2.
	     MOVE SHADOW-DETAIL
				 TO SHADOW-DETAIL2.
	     MOVE CRT-DETAIL	 TO CRT-DETAIL2.
	   IF WS-OPTION = "1"
	       PERFORM FB000
	   ELSE
	   IF WS-OPTION = "2"
	       PERFORM FC000
	   ELSE
	   IF WS-OPTION = "3"
	       PERFORM FD000
	   ELSE
	   IF WS-OPTION = "4"
	       PERFORM FE000.
	     PERFORM RESTORE-SCREEN-2.
	     MOVE BLOCK-DETAIL2  TO BLOCK-DETAIL.
	     MOVE SHADOW-DETAIL2 TO SHADOW-DETAIL.
	     MOVE CRT-DETAIL2	 TO CRT-DETAIL.
	     GO TO FA05.

       FA999.
	     EXIT.

      /
      *    ****   J O U R N A L   B A T C H   T R A N S A C T I O N S
      *
       FB000	       SECTION 53.
       FB00.
	   IF LS-DSKDRV = "L"
	       MOVE "Not available on Local drive"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO FB999.
      *
      *    This is a temporary test which can be removed once the
      *    FA000 Section is performed.
      *
	   IF LS0-LEV < 2
	       MOVE "MNU\NOPMENU"
				 TO PRG-NAME
	       PERFORM AA100
	       DISPLAY MENU-INSTRUCT
	       GO TO FB999.
	     MOVE 9		 TO SHADE-ROW.
	     MOVE 51		 TO SHADE-COL.
	     MOVE 24		 TO SHADE-WIDTH.
	     MOVE 8		 TO SHADE-LINES.
	     DISPLAY S11.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.
	     MOVE 9		 TO TOP-ROW.
	     MOVE 14		 TO BOTTOM-ROW.
	     MOVE 8		 TO SCREEN-LIN.
	     MOVE 51		 TO SCREEN-COL.
	     MOVE 22		 TO STRING-LENGTH.
	     PERFORM SAVE-ATTR.

       FB05.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       FB10.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY	 PERFORM MOVE-BLOCK-UP
		 WHEN DOWN-KEY	 PERFORM MOVE-BLOCK-DOWN
		 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
				 IF MOUSE-ENTER = "Y"
				     MOVE "N"	 TO MOUSE-ENTER
				     GO TO FB15
				 END-IF
				 IF MOUSE-Y = 24
				     IF MOUSE-X = 5
					 MOVE 5  TO KEY-CODE-1
					 GO TO FB10
				     ELSE
				     IF MOUSE-X = 10
					 MOVE 6  TO KEY-CODE-1
					 GO TO FB10
				     ELSE
				     IF MOUSE-X > 36 AND < 44
					 MOVE 0  TO KEY-CODE-1
					 GO TO FB10
				     ELSE
				     IF MOUSE-X > 54 AND < 60
					 MOVE 0  TO KEY-CODE-1
					 MOVE 1  TO KEY-TYPE
					 GO TO FB10
				     END-IF
				 END-IF
		 WHEN ENTER-KEY  GO TO FB15
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO FB05
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE "0"	 TO WS-OPTION
		     GO TO FB18
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO FB05
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	       IF WS-OPTION < 0 OR > 5
		   CALL X"E5"
		   GO TO FB05
	       END-IF
	       GO TO FB16
	   ELSE
	       CALL X"E5"
	       GO TO FB05.

       FB15.
	   IF SCREEN-LIN < 9
	       CALL X"E5"
	       GO TO FB05
	   ELSE
	   IF SCREEN-LIN = 9
	       MOVE "1"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 10
	       MOVE "2"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 11
	       MOVE "3"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 12
	       MOVE "4"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 13
	       MOVE "5"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 14
	       MOVE "0"		 TO WS-OPTION.
	   GO TO FB18.

       FB16.
	     PERFORM CLEAR-BLOCK.
	   IF WS-OPTION = 0
	       MOVE 14		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = 1
	       MOVE 9		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = 2
	       MOVE 10		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = 3
	       MOVE 11		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = 4
	       MOVE 12		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = 5
	       MOVE 13		 TO SCREEN-LIN.
	     PERFORM SAVE-ATTR.
	     PERFORM MARK-BLOCK.

       FB18.
	   IF WS-OPTION = "0"
	       GO TO FB999.
	   IF WS-OPTION < 1 OR > 5
	       CALL X"E5"
	       GO TO FB05.
	   IF WS-OPTION = "1"
	       MOVE "GLP\GLP061" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "2"
	       MOVE "GLP\GLP062" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "3"
	       MOVE "GLP\GLP063" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "4"
	       MOVE "GLP\GLP064" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "5"
	       MOVE "GLP\GLP065" TO PRG-NAME.
	     PERFORM AA100.
	     DISPLAY MENU-INSTRUCT.
	     GO TO FB05.

       FB999.
	     EXIT.
      /
      *    ****   C H E Q U E S   B A T C H   T R A N S A C T I O N S
      *
       FC000	       SECTION 53.
       FC00.
	     MOVE 9		 TO SHADE-ROW.
	     MOVE 51		 TO SHADE-COL.
	     MOVE 24		 TO SHADE-WIDTH.
	     MOVE 8		 TO SHADE-LINES.
	     DISPLAY S11.
	     DISPLAY "CHEQUE - BATCHES" AT 0954
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.
	     MOVE 9		 TO TOP-ROW.
	     MOVE 14		 TO BOTTOM-ROW.
	     MOVE 8		 TO SCREEN-LIN.
	     MOVE 51		 TO SCREEN-COL.
	     MOVE 22		 TO STRING-LENGTH.
	     PERFORM SAVE-ATTR.

       FC05.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       FC10.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY	 PERFORM MOVE-BLOCK-UP
		 WHEN DOWN-KEY	 PERFORM MOVE-BLOCK-DOWN
		 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
				 IF MOUSE-ENTER = "Y"
				     MOVE "N"	 TO MOUSE-ENTER
				     GO TO FC15
				 END-IF
				 IF MOUSE-Y = 24
				     IF MOUSE-X = 5
					 MOVE 5  TO KEY-CODE-1
					 GO TO FC10
				     ELSE
				     IF MOUSE-X = 10
					 MOVE 6  TO KEY-CODE-1
					 GO TO FC10
				     ELSE
				     IF MOUSE-X > 36 AND < 44
					 MOVE 0  TO KEY-CODE-1
					 GO TO FC10
				     ELSE
				     IF MOUSE-X > 54 AND < 60
					 MOVE 0  TO KEY-CODE-1
					 MOVE 1  TO KEY-TYPE
					 GO TO FC10
				     END-IF
				 END-IF
		 WHEN ENTER-KEY  GO TO FC15
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO FC05
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE "0"	 TO WS-OPTION
		     GO TO FC18
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO FC05
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	       IF WS-OPTION < 0 OR > 5
		   CALL X"E5"
		   GO TO FC05
	       END-IF
	       GO TO FC16
	   ELSE
	       CALL X"E5"
	       GO TO FC05.

       FC15.
	   IF SCREEN-LIN < 9
	       CALL X"E5"
	       GO TO FC05
	   ELSE
	   IF SCREEN-LIN = 9
	       MOVE "1"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 10
	       MOVE "2"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 11
	       MOVE "3"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 12
	       MOVE "4"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 13
	       MOVE "5"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 14
	       MOVE "0"		 TO WS-OPTION.
	   GO TO FC18.

       FC16.
	     PERFORM CLEAR-BLOCK.
	   IF WS-OPTION = 0
	       MOVE 14		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = 1
	       MOVE 9		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = 2
	       MOVE 10		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = 3
	       MOVE 11		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = 4
	       MOVE 12		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = 5
	       MOVE 13		 TO SCREEN-LIN.
	     PERFORM SAVE-ATTR.
	     PERFORM MARK-BLOCK.

       FC18.
	   IF WS-OPTION = "0"
	       GO TO FC999.
	   IF WS-OPTION < 1 OR > 5
	       CALL X"E5"
	       GO TO FC05.
	   IF WS-OPTION = "1"
	       MOVE "GLP\GLP071" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "2"
	       MOVE "GLP\GLP072" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "3"
	       MOVE "GLP\GLP073" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "4"
	       MOVE "GLP\GLP074" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "5"
	       MOVE "GLP\GLP075" TO PRG-NAME.
	     PERFORM AA100.
	     DISPLAY MENU-INSTRUCT.
	     GO TO FC05.

       FC999.
	     EXIT.
      /
      *    ****   D E P O S I T   B A T C H   T R A N S A C T I O N S
      *
       FD000	       SECTION 53.
       FD00.
	     MOVE 9		 TO SHADE-ROW.
	     MOVE 51		 TO SHADE-COL.
	     MOVE 24		 TO SHADE-WIDTH.
	     MOVE 8		 TO SHADE-LINES.
	     DISPLAY S11.
	     DISPLAY "DEPOSIT -BATCHES" AT 0954
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.
	     MOVE 9		 TO TOP-ROW.
	     MOVE 14		 TO BOTTOM-ROW.
	     MOVE 8		 TO SCREEN-LIN.
	     MOVE 51		 TO SCREEN-COL.
	     MOVE 22		 TO STRING-LENGTH.
	     PERFORM SAVE-ATTR.

       FD05.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       FD10.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY	 PERFORM MOVE-BLOCK-UP
		 WHEN DOWN-KEY	 PERFORM MOVE-BLOCK-DOWN
		 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
				 IF MOUSE-ENTER = "Y"
				     MOVE "N"	 TO MOUSE-ENTER
				     GO TO FD15
				 END-IF
				 IF MOUSE-Y = 24
				     IF MOUSE-X = 5
					 MOVE 5  TO KEY-CODE-1
					 GO TO FD10
				     ELSE
				     IF MOUSE-X = 10
					 MOVE 6  TO KEY-CODE-1
					 GO TO FD10
				     ELSE
				     IF MOUSE-X > 36 AND < 44
					 MOVE 0  TO KEY-CODE-1
					 GO TO FD10
				     ELSE
				     IF MOUSE-X > 54 AND < 60
					 MOVE 0  TO KEY-CODE-1
					 MOVE 1  TO KEY-TYPE
					 GO TO FD10
				     END-IF
				 END-IF
		 WHEN ENTER-KEY  GO TO FD15
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO FD05
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE "0"	 TO WS-OPTION
		     GO TO FD18
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO FD05
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	       IF WS-OPTION < 0 OR > 5
		   CALL X"E5"
		   GO TO FD05
	       END-IF
	       GO TO FD16
	   ELSE
	       CALL X"E5"
	       GO TO FD05.

       FD15.
	   IF SCREEN-LIN < 9
	       CALL X"E5"
	       GO TO FD05
	   ELSE
	   IF SCREEN-LIN = 9
	       MOVE "1"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 10
	       MOVE "2"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 11
	       MOVE "3"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 12
	       MOVE "4"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 13
	       MOVE "5"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 14
	       MOVE "0"		 TO WS-OPTION.
	   GO TO FD18.

       FD16.
	     PERFORM CLEAR-BLOCK.
	   IF WS-OPTION = 0
	       MOVE 14		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = 1
	       MOVE 9		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = 2
	       MOVE 10		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = 3
	       MOVE 11		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = 4
	       MOVE 12		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = 5
	       MOVE 13		 TO SCREEN-LIN.
	     PERFORM SAVE-ATTR.
	     PERFORM MARK-BLOCK.

       FD18.
	   IF WS-OPTION = "0"
	       GO TO FD999.
	   IF WS-OPTION < 1 OR > 5
	       CALL X"E5"
	       GO TO FD05.
	   IF WS-OPTION = "1"
	       MOVE "GLP\GLP081" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "2"
	       MOVE "GLP\GLP082" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "3"
	       MOVE "GLP\GLP083" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "4"
	       MOVE "GLP\GLP084" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "5"
	       MOVE "GLP\GLP085" TO PRG-NAME.
	     PERFORM AA100.
	     DISPLAY MENU-INSTRUCT.
	     GO TO FD05.

       FD999.
	     EXIT.

       FE000	       SECTION 53.
       FE00.
	     MOVE "GLP\GLPENQ"	 TO PRG-NAME.
	     PERFORM AA100.

       FE999.
	     EXIT.

      /
      *    ****    S T A N D I N G   E N T R I E S
      *
       GA000	       SECTION 54.
       GA00.
	   IF LS-DSKDRV = "L"
	       MOVE "Not available on Local drive"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO GA999.
	   IF LS0-LEV < 2
	       MOVE "MNU\NOPMENU"
				 TO PRG-NAME
	       PERFORM AA100
	       DISPLAY MENU-INSTRUCT
	       GO TO GA999.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 29		 TO SHADE-COL.
	     MOVE 24		 TO SHADE-WIDTH.
	     MOVE 8		 TO SHADE-LINES.
	     DISPLAY S08.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.
	     MOVE 7		 TO TOP-ROW.
	     MOVE 12		 TO BOTTOM-ROW.
	     MOVE 6		 TO SCREEN-LIN.
	     MOVE 29		 TO SCREEN-COL.
	     MOVE 22		 TO STRING-LENGTH.
	     PERFORM SAVE-ATTR.

       GA05.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       GA10.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY	 PERFORM MOVE-BLOCK-UP
		 WHEN DOWN-KEY	 PERFORM MOVE-BLOCK-DOWN
		 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
				 IF MOUSE-ENTER = "Y"
				     MOVE "N"	 TO MOUSE-ENTER
				     GO TO GA15
				 END-IF
				 IF MOUSE-Y = 24
				     IF MOUSE-X = 5
					 MOVE 5  TO KEY-CODE-1
					 GO TO GA10
				     ELSE
				     IF MOUSE-X = 10
					 MOVE 6  TO KEY-CODE-1
					 GO TO GA10
				     ELSE
				     IF MOUSE-X > 36 AND < 44
					 MOVE 0  TO KEY-CODE-1
					 GO TO GA10
				     ELSE
				     IF MOUSE-X > 54 AND < 60
					 MOVE 0  TO KEY-CODE-1
					 MOVE 1  TO KEY-TYPE
					 GO TO GA10
				     END-IF
				 END-IF
		 WHEN ENTER-KEY  GO TO GA15
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO GA05
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE "0"	 TO WS-OPTION
		     GO TO GA18
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO GA05
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	       IF WS-OPTION < 0 OR > 5
		   CALL X"E5"
		   GO TO GA05
	       END-IF
	       GO TO GA16
	   ELSE
	       CALL X"E5"
	       GO TO GA05.

       GA15.
	   IF SCREEN-LIN < 7
	       CALL X"E5"
	       GO TO GA05
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
	   GO TO GA18.

       GA16.
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
	       MOVE 11		 TO SCREEN-LIN.
	     PERFORM SAVE-ATTR.
	     PERFORM MARK-BLOCK.

       GA18.
	   IF WS-OPTION = "0"
	       GO TO GA999.
	   IF WS-OPTION < 1 OR > 5
	       CALL X"E5"
	       GO TO GA05.
	   IF WS-OPTION = "1"
	       MOVE "GLP\GLP041" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "2"
	       MOVE "GLP\GLP042" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "3"
	       MOVE "GLP\GLP043" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "4"
	       MOVE "GLP\GLP044" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "5"
	       MOVE "GLP\GLP045" TO PRG-NAME.
	     PERFORM AA100.
	     DISPLAY MENU-INSTRUCT.
	     GO TO GA05.

       GA999.
	     EXIT.
      /
      *    ****    L E D G E R	 E X P O R T S
      *
       HA000	       SECTION 55.
       HA00.
	   IF LS-DSKDRV = "L"
	       MOVE "Not available on Local drive"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO HA999.
	   IF LS0-LEV < 2
	       MOVE "MNU\NOPMENU"
				 TO PRG-NAME
	       PERFORM AA100
	       GO TO HA999.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 31		 TO SHADE-COL.
	     MOVE 20		 TO SHADE-WIDTH.
	     MOVE 5		 TO SHADE-LINES.
	     DISPLAY S12.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.
	     MOVE 7		 TO TOP-ROW.
	     MOVE 9		 TO BOTTOM-ROW.
	     MOVE 6		 TO SCREEN-LIN.
	     MOVE 31		 TO SCREEN-COL.
	     MOVE 18		 TO STRING-LENGTH.
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
	       IF WS-OPTION < 0 OR > 2
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
	       MOVE "0"		 TO WS-OPTION.
	   GO TO HA18.

       HA16.
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

       HA18.
	   IF WS-OPTION = "0"
	       GO TO HA999.
	     PERFORM SAVE-SCREEN-2.
	     MOVE BLOCK-DETAIL	 TO BLOCK-DETAIL2.
	     MOVE SHADOW-DETAIL
				 TO SHADOW-DETAIL2.
	     MOVE CRT-DETAIL	 TO CRT-DETAIL2.
	   IF WS-OPTION = "1"
	       MOVE "GLP\GLPEXP" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "2"
	       MOVE "GLP\GLPEXPT"
				 TO PRG-NAME.
	     PERFORM AA100.
	     DISPLAY MENU-INSTRUCT.
	     GO TO HA05.

       HA999.
	     EXIT.
      /
      *    ****   L E D G E R	U T I L I T I E S
      *
       IA000	       SECTION 56.
       IA00.
	   IF LS-DSKDRV = "L"
	       MOVE "Not available on Local drive"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO IA999.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 29		 TO SHADE-COL.
	     MOVE 24		 TO SHADE-WIDTH.
	     MOVE 9		 TO SHADE-LINES.
	     DISPLAY S90.
	   IF LS0-LEV < 3
	       DISPLAY "N/A" AT 0849
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 3.
	   IF LS0-LEV < 4
	       DISPLAY "t N/A" AT 1247
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 3
	       DISPLAY "ch N/A" AT 1346
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 3.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.
	     MOVE 7		 TO TOP-ROW.
	     MOVE 13		 TO BOTTOM-ROW.
	     MOVE 6		 TO SCREEN-LIN.
	     MOVE 29		 TO SCREEN-COL.
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
	       IF WS-OPTION < 0 OR > 6 AND
		  WS-OPTION NOT = ">"  AND
		  WS-OPTION NOT = "^"  AND
		  WS-OPTION NOT = "<"  AND
		  WS-OPTION NOT = "\"	AND
		  WS-OPTION NOT = "D"  AND
		  WS-OPTION NOT = "T"
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
	       MOVE "0"		 TO WS-OPTION.
	   GO TO IA18.

       IA16.
	     PERFORM CLEAR-BLOCK.
	   IF WS-OPTION = 0
	       MOVE 13		 TO SCREEN-LIN
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
	   IF WS-OPTION = 6
	       MOVE 12		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = ">" OR "T" OR "^" OR "<" OR "\" OR "D"
	       MOVE 12		 TO SCREEN-LIN.
	     PERFORM SAVE-ATTR.
	     PERFORM MARK-BLOCK.

       IA18.
	   IF WS-OPTION = "0"
	       GO TO IA999.
	   IF WS-OPTION < 1 OR > 6 AND
	      WS-OPTION NOT = ">"  AND
	      WS-OPTION NOT = "^"  AND
	      WS-OPTION NOT = "<"  AND
	      WS-OPTION NOT = "\"  AND
	      WS-OPTION NOT = "D"  AND
	      WS-OPTION NOT = "T"
	       CALL X"E5"
	       GO TO IA05.
	   IF WS-OPTION = "1"
	       IF LS0-LEV < 3
		   MOVE "MNU\NOPMENU" TO PRG-NAME
	       ELSE
		   MOVE "GLP\GLP091" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "2"
	       MOVE "GLP\GLP092" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "3"
	       MOVE "GLP\GLP093" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "4"
	       MOVE "GLP\GLPENQ" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "5"
	       IF LS0-LEV < 4
		   MOVE "MNU\NOPMENU" TO PRG-NAME
	       ELSE
		   MOVE "GLP\GLP095" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "6"
	       IF LS0-LEV < 4
		   MOVE "MNU\NOPMENU" TO PRG-NAME
	       ELSE
		   MOVE "GLP\GLP097" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "^"
	       MOVE "GLP\GLP099" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = ">"
	       MOVE "GLP\GLP096" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "<"
	       MOVE "GLP\GLPTPTCH"
				 TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "\"
	       MOVE "GLP\GLPTRFER"
				 TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "D"
	       MOVE "GLP\GLTDEL" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "T"
	       MOVE "GLP\GLPTNO" TO PRG-NAME
	       PERFORM AA100
	       DISPLAY MENU-INSTRUCT
	       MOVE "GLP\GLP092" TO PRG-NAME.
	     PERFORM AA100.
	     DISPLAY MENU-INSTRUCT.
	     GO TO IA05.

       IA999.
	     EXIT.
      /
      *    ****    L E D G E R	 E N Q U I R I E S
      *
       JA000	       SECTION 57.
       JA00.
	     MOVE "GLP\GLPENQ"	 TO PRG-NAME.
	     PERFORM AA100.

       JA999.
	     EXIT.
      /
      *    ****    L E D G E R	 S E T U P   B O O K S
      *
       KA000	       SECTION 57.
       KA00.
	   IF LS-DSKDRV = "L"
	       MOVE "Not available on Local drive"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO KA999.
	     MOVE "GLP\GLPBKS"	 TO PRG-NAME.
	     PERFORM AA100.

       KA999.
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
	     MOVE "GACCNT"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-GACCNT.
	     MOVE "GLTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-GLTRAN.
	     MOVE "LEDTRF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-LEDTRF.
035470	     OPEN I-O GACCNT.
035480     IF RUNTIME-ERROR
               IF FLE-LOCKED
042030             GO TO ZA200
               ELSE
               IF FLE-LIMIT
		   GO TO ZA49
               ELSE
	       IF IDX-CORRUPT
		   MOVE 8	 TO WS-F-ERROR
                   GO TO ZA50.
035500     IF WS-STATUS NOT = "00"
035510	       MOVE 8		 TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
050480		   PERFORM OPEN-ERROR.
035530       OPEN I-O GLTRAN.
035540     IF RUNTIME-ERROR
               IF FLE-LOCKED
042030             GO TO ZA200
               ELSE
               IF FLE-LIMIT
		   GO TO ZA49
               ELSE
	       IF IDX-CORRUPT
		   MOVE 11	 TO WS-F-ERROR
		   GO TO ZA50.
035560     IF WS-STATUS NOT = "00"
035570	       MOVE 11		 TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
050480		   PERFORM OPEN-ERROR.
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
050190     IF WS-STATUS NOT = "00"
050200         MOVE 40           TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
050210		   PERFORM OPEN-ERROR.
	   IF LS-DEM = 4
	       MOVE "GLP\GLP091" TO PRG-NAME
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
041850	   IF WS-F-ERROR = 8
041860	       MOVE "G/L ACCOUNT file"
				 TO WS-FILE
	   ELSE
041850	   IF WS-F-ERROR = 11
041860	       MOVE "G/L TRANSACTION"
				 TO WS-FILE
	   ELSE
041850	   IF WS-F-ERROR = 40
041860         MOVE "INTEGRATION"
                                 TO WS-FILE.
039980	     DISPLAY "Rebuild " AT 0812
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
041810	   IF WS-F-ERROR = 8
041820         MOVE W02-GACCNT TO WS-FILE
041830         MOVE WS-GLKEY TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 11
041860         MOVE W02-GLTRAN   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
               MOVE T-KEY        TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 40
041860         MOVE W02-LEDTRF   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE XFR-KEY	 TO WS-KEYX.
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
