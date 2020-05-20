      $set LINKCOUNT"576"
      ******************************************************************
      *                                                                *
      * *******  ******** *******  **	  ** ******* **	    ** **   ** *
      * **    **    **	  **	** ***	 *** **      ***    ** **   ** *
      * **    **    **	  **	** ** * * ** **      ** *   ** **   ** *
      * **    **    **	  *******  **  *  ** *****   **  *  ** **   ** *
      * **    **    **	  **	   **	  ** **	     **	  * ** **   ** *
      * **    **    **	  **	   **	  ** **	     **	   *** **   ** *
      * *******     **	  **	   **	  ** ******* **	    **	*****  *
      * 							       *
      *     ENGLISH                                                    *
      *                                                                *
      *     D E B T O R S   M O D U L E   M E N U   P R O G R A M      *
      *                                                                *
      *     VERSION 8.15.04 - January 2012			       *
      * 							       *
      ******************************************************************
      * 							       *
      *  May 2006	- New field included in Accounts Receivable    *
      * 		  for recording number of outstanding Jobs     *
      * 		  linked to the account. Additional filler     *
      * 		  added for future expansion.		       *
      * 							       *
      *  February 2007	- New file (DEBDEP/AR_REST.DAT) - Debtor sales *
      * 		  restrictions file has been included which    *
      * 		  will allow for departments to de excluded or *
      * 		  only selected departments allowed. This will *
      * 		  allow the user the abillity to allow or bar  *
      * 		  sales of selected departments.	       *
      * 							       *
      *  January 2008	- New file (DEBALT) - Debtor alternate index   *
      * 		  included for lookups, using any word con-    *
      * 		  tained in the Debtor name.		       *
      * 							       *
      *  October 2008	- Amended Accounts receivable ageing to create *
      * 		  duplicate files of the accounts and trans-   *
      * 		  actions for printing of copy satements when  *
      * 		  and if required. The file names are AR_Ayymm *
      * 		  and AR_Tyymm, where yymm is replaced with    *
      * 		  the year and month.			       *
      * 							       *
      *  March 2010	- Include facility to print to a USB printer   *
      * 		  via the Windows print using the relavent     *
      * 		  print API's supplied with MF COBOL.          *
      * 							       *
      ******************************************************************
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		    VERSIONS OF THE APAC SYSTEM		     ³
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
000020 PROGRAM-ID.     DTPMENU.
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

       COPY CONTROL.SL.

000160 COPY DBTRAN.SL.

000160 COPY DEBALT.SL.

000140 COPY DEBTOR.ISL.

000140 COPY DEBMEM.SL.

000200 COPY INVOIC.SL.

       COPY LEDTRF.SL.

000210 COPY PARAM.SL.

000330 COPY STOCK.SL.

      /
000330 DATA DIVISION.
000340 FILE SECTION.

       COPY APACFIDS.FDE.

000360 COPY CONTROL.FDE.

000330 COPY DBTRAN.FDE.

000330 COPY DEBALT.FDE.

000310 COPY DEBTOR.FDE.

000140 COPY DEBMEM.FDE.

000370 COPY INVOIC.FDE.

       COPY LEDTRF.FDE.

000440 COPY PARAM.FDE.

000620 COPY STOCK.FDE.

      /
000560 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
000610 77  WS-S1	   PIC	9(04)	 COMP-5.
000620 77  WS-S2           PIC  9(04)    COMP-5.
000630 77  WS-S3           PIC  9(04)    COMP-5.
000640 77  WS-S4           PIC  9(04)    COMP-5.
       77  WS-TEMP-LIN	   PIC	9(02)	 COMP-5.
003290 77  WS-NETKEY        PIC 9(04)     COMP-5 VALUE 1.
002900 77  WS-PARKEY	   PIC	9(04)	 COMP-5.
       77  WS-OPTION	   PIC	X(01).
       77  WS-REOPEN	   PIC	X(01) VALUE SPACE.
       77  PRG-NAME	   PIC	X(12) VALUE SPACES.
       77  TODAY-DDMMYY	       PIC  9(08) COMP-5.

       01  PRG-STATEMENT.
	   03  FILLER	   PIC	X(08) VALUE "DTP\STMF".
	   03  PRG-NUMBER  PIC	9(02) VALUE ZERO.

       01  PRG-WSTATEMENT.
	   03  FILLER	   PIC	X(11) VALUE "DTP\STMF07W".

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

000290 COPY CONTROL.ID.

       COPY DBTRAN.ID.

000330 COPY DEBALT.ID.

       COPY DEBTOR.ID.

000140 COPY DEBMEM.ID.

       COPY INVOIC.ID.

       COPY LEDTRF.ID.

       COPY PARAM.ID.

003860 COPY STOCK.ID.

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
001450	   03  LINE  2 COLUMN 28 BACKGROUND-COLOR 1
				 FOREGROUND-COLOR 7 HIGHLIGHT
001460				 VALUE "ACCOUNTS RECEIVABLE MODULE".

       01  S02.
	   03  LINE  4 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
	   03	       COLUMN 29 BACKGROUND-COLOR 2 FOREGROUND-COLOR 0
				 VALUE "¿".
	   03  LINE  5 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³   ".
	   03	       COLUMN 11 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "DEBTOR PROGRAMS".
	   03	       COLUMN 26 BACKGROUND-COLOR 2
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
				 VALUE " Maintenance       ³".
001470	   03  LINE  7 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "2".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Transactions      ³".
001470	   03  LINE  8 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "3".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Reports/Statements³".
001470	   03  LINE  9 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "4".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Collection Enquiry³".
001570	   03  LINE 10 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "5".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Export Options    ³".
	   03  LINE 11 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "6".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Lookup/RePrint Inv³".
001570	   03  LINE 12 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "7".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Utilities         ³".
001570	   03  LINE 13 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "8".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Rentals/Service   ³".
001570	   03  LINE 14 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "A".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Enquiry           ³".
001570	   03  LINE 15 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "B".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Sales Tracking    ³".
001570	   03  LINE 16 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "C".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Import Options    ³".
001500	   03  LINE 17 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "D".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Purchase History  ³".
001500	   03  LINE 18 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "E".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Client Price List ³".
001500	   03  LINE 19 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "F".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Payments Received ³".
001500	   03  LINE 20 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "G".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Enquiry (History) ³".
001500	   03  LINE 21 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "0".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Exit	           ³".
	   03  LINE 22 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "À".
	   03	       COLUMN  8 BACKGROUND-COLOR 2 FOREGROUND-COLOR 0
				 VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S03.
	   03  LINE  6 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
	   03	       COLUMN 50 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
				 VALUE "¿".
	   03  LINE  7 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³      ".
	   03	       COLUMN 33 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "MAINTENANCE".
	   03	       COLUMN 44 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE "      ³".
	   03  LINE  8 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "1".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Add a New Account   ³".
001470	   03  LINE  9 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "2".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Amend an Account    ³".
001570	   03  LINE 10 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "3".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Delete an Account   ³".
001570	   03  LINE 11 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "4".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Amend Account No.   ³".
001570	   03  LINE 12 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "5".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Memo/Remarks Maint  ³".
001570	   03  LINE 13 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "6".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Age Accounts        ³".
001570	   03  LINE 14 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "7".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Flag as Bad Debt    ³".
001570	   03  LINE 15 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "8".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Sales Restrictions  ³".
001570	   03  LINE 16 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "A".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Account Enquiry     ³".
001570	   03  LINE 17 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "B".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " A/C Enquiry (Hist)  ³".
001570	   03  LINE 18 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "C".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Balance Take-On     ³".
001570	   03  LINE 19 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "0".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Exit	             ³".
	   03  LINE 20 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "À".
	   03	       COLUMN 27 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
				 VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S04.
	   03  LINE  6 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
	   03	       COLUMN 49 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
				 VALUE "¿".
	   03  LINE  7 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³     ".
	   03	       COLUMN 32 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "TRANSACTIONS".
	   03	       COLUMN 44 BACKGROUND-COLOR 3
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
				 VALUE " Invoices (Analysed)³".
001470	   03  LINE  9 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "2".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Cr Notes (Analysed)³".
001570	   03  LINE 10 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "3".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Sundry Transactions³".
001570	   03  LINE 11 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "4".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Arrear Interest    ³".
001570	   03  LINE 12 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "5".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Receipts (POS)     ³".
001570	   03  LINE 13 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "6".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Allocate Credits   ³".
001570	   03  LINE 14 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "7".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Account Enquiry    ³".
001570	   03  LINE 15 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "8".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Sales Tracking Enq ³".
001570	   03  LINE 16 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "A".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Allocate Credits 2 ³".
001570	   03  LINE 17 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "B".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Batch Transactions ³".
001570	   03  LINE 18 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "0".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Exit	            ³".
	   03  LINE 19 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "À".
	   03	       COLUMN 27 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
				 VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S05.
	   03  LINE  6 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
	   03	       COLUMN 50 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
				 VALUE "¿".
	   03  LINE  7 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "REPORTS & STATEMENTS".
	   03	       COLUMN 48 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE "  ³".
	   03  LINE  8 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "1".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Period End Reports  ³".
001470	   03  LINE  9 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "2".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Statistical Reports ³".
001570	   03  LINE 10 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "3".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Sundry Reports      ³".
001570	   03  LINE 11 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "0".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Exit	             ³".
	   03  LINE 12 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "À".
	   03	       COLUMN 27 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
				 VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".


       01  S06A.
	   03  LINE  8 COLUMN 48 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
	   03	       COLUMN 72 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE "¿".
	   03  LINE  9 COLUMN 48 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³    ".
	   03	       COLUMN 53 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "AR - PERIOD END".
	   03	       COLUMN 68 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE "    ³".
	   03  LINE 10 COLUMN 48 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 50 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "1".
	   03	       COLUMN 51 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Age Analysis        ³".
	   03  LINE 11 COLUMN 48 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 50 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "2".
	   03	       COLUMN 51 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Detailed Accounts   ³".
	   03  LINE 12 COLUMN 48 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 50 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "3".
	   03	       COLUMN 51 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Statements          ³".
	   03  LINE 13 COLUMN 48 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 50 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "4".
	   03	       COLUMN 51 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Transaction Journal ³".
	   03  LINE 14 COLUMN 48 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 50 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "5".
	   03	       COLUMN 51 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Trial Balance       ³".
	   03  LINE 15 COLUMN 48 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 50 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "6".
	   03	       COLUMN 51 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Unallocated Credits ³".
	   03  LINE 16 COLUMN 48 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 50 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "7".
	   03	       COLUMN 51 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Statement: USB/Wind ³".
	   03  LINE 17 COLUMN 48 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 50 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "0".
	   03	       COLUMN 51 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Exit                ³".
	   03  LINE 18 COLUMN 48 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "À".
	   03	       COLUMN 49 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S06B.
	   03  LINE  8 COLUMN 48 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
	   03	       COLUMN 73 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE "¿".
	   03  LINE  9 COLUMN 48 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³".
	   03	       COLUMN 49 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "AR - STATISTICAL REPORTS".
	   03	       COLUMN 73 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE "³".
	   03  LINE 10 COLUMN 48 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 50 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "1".
	   03	       COLUMN 51 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Accounts per Rep.    ³".
	   03  LINE 11 COLUMN 48 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 50 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "2".
	   03	       COLUMN 51 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Clients per Category ³".
	   03  LINE 12 COLUMN 48 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 50 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "3".
	   03	       COLUMN 51 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " G.P. per Account     ³".
	   03  LINE 13 COLUMN 48 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 50 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "4".
	   03	       COLUMN 51 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Purchases Statistics ³".
	   03  LINE 14 COLUMN 48 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 50 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "5".
	   03	       COLUMN 51 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Sales per Account    ³".
	   03  LINE 15 COLUMN 48 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 50 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "6".
	   03	       COLUMN 51 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Stats per Acnt/Depart³".
	   03  LINE 16 COLUMN 48 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 50 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "0".
	   03	       COLUMN 51 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Exit                 ³".
	   03  LINE 17 COLUMN 48 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "À".
	   03	       COLUMN 49 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".


       01  S06C.
	   03  LINE  8 COLUMN 48 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
	   03	       COLUMN 73 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE "¿".
	   03  LINE  9 COLUMN 48 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³  ".
	   03	       COLUMN 51 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "AR - SUNDRY REPORTS".
	   03	       COLUMN 70 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE "   ³".
	   03  LINE 10 COLUMN 48 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 50 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "1".
	   03	       COLUMN 51 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Account/Discount Rep.³".
	   03  LINE 11 COLUMN 48 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 50 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "2".
	   03	       COLUMN 51 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Address Labels       ³".
	   03  LINE 12 COLUMN 48 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 50 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "3".
	   03	       COLUMN 51 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Alphabetic List      ³".
	   03  LINE 13 COLUMN 48 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 50 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "4".
	   03	       COLUMN 51 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Client Accounts List ³".
	   03  LINE 14 COLUMN 48 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 50 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "5".
	   03	       COLUMN 51 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Unallocated Credits  ³".
	   03  LINE 15 COLUMN 48 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 50 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "0".
	   03	       COLUMN 51 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Exit                 ³".
	   03  LINE 16 COLUMN 48 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "À".
	   03	       COLUMN 49 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S07.
	   03  LINE  6 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
	   03	       COLUMN 50 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
				 VALUE "¿".
	   03  LINE  7 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³       ".
	   03	       COLUMN 34 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "UTILITIES".
	   03	       COLUMN 43 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE "       ³".
	   03  LINE  8 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "1".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Clear Balances      ³".
001470	   03  LINE  9 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "2".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Recalculate Balances³".
001570	   03  LINE 10 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "3".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Delete Invoices     ³".
001570	   03  LINE 11 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "4".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Account Enquiry     ³".
001570	   03  LINE 12 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "5".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Set Address Flag    ³".
001570	   03  LINE 13 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "6".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Check Transaction   ³".
001470	   03  LINE 14 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "7".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Recalc. Purchases   ³".
001470	   03  LINE 15 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "8".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Set Month End Day   ³".
001470	   03  LINE 16 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "A".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Fix Month End Dates ³".
001470	   03  LINE 17 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "B".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Create ALT NAME Idx ³".
001570	   03  LINE 18 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "0".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Exit	             ³".
	   03  LINE 19 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "À".
	   03	       COLUMN 27 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
				 VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S08.
	   03  LINE  6 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
	   03	       COLUMN 50 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
				 VALUE "¿".
	   03  LINE  7 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³    ".
	   03	       COLUMN 31 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "RENTALS/SERVICE".
	   03	       COLUMN 46 BACKGROUND-COLOR 3
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
				 VALUE " Contract Maintenance³".
001470	   03  LINE  9 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "2".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Reports             ³".
001570	   03  LINE 10 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "3".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Monthly Readings    ³".
001570	   03  LINE 11 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "4".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Account Enquiry     ³".
001570	   03  LINE 12 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "5".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Agreement Enquiry   ³".
001570	   03  LINE 13 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "6".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Monthly Posting     ³".
001570	   03  LINE 14 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "7".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Delete Contract     ³".
001570	   03  LINE 15 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "0".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Exit	             ³".
	   03  LINE 16 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "À".
	   03	       COLUMN 27 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
				 VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       COPY ERROR.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
       AA000	       SECTION.
       AA00.
	     PERFORM ZA000-INIT.
	     DISPLAY CLR-L1-2.
	     DISPLAY S01.
003240	   IF LS0-DTP NOT = 2
	       MOVE "Not Installed"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	   ELSE
003240	   IF LS0-DEBT = "N"
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	   ELSE
	       PERFORM BA000
	   END-IF.
	   IF MOUSE-ATTACHED = "Y"
	       MOVE 66		 TO MOUSE-FUNC
	       MOVE 0		 TO MOUSE-PARAM
	       CALL X"AF" USING MOUSE-FUNC
				MOUSE-PARAM
	   END-IF.
043610	     CLOSE DEBALT
		   DEBTOR
                   DEBMEM
043630             DBTRAN
		   INVOICE
043660		   LEDTRF.
013140 AA99.
013150       EXIT PROGRAM.

       AA100	       SECTION.
       AA101.
	   IF MOUSE-ATTACHED = "Y"
	       MOVE 66		 TO MOUSE-FUNC
	       MOVE 0		 TO MOUSE-PARAM
	       CALL X"AF" USING MOUSE-FUNC
				MOUSE-PARAM
	   END-IF.
	     PERFORM SAVE-SCREEN-3.
	     MOVE SPACES       TO LS-PRG-NAME.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Strip the path from the program name and put the program  ³
      *    ³ name into LS-PRG-NAME.				       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     UNSTRING PRG-NAME DELIMITED BY "\" OR " "
		      INTO WS-STAT-MESSAGE LS-PRG-NAME.
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
				MOUSE-PARAM
	   END-IF.
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
	     MOVE 21		 TO SHADE-WIDTH.
	     MOVE 18		 TO SHADE-LINES.
	     DISPLAY S02.
	   IF LS0-LEV < 2
	       DISPLAY " N/A" AT 0925
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
	       DISPLAY "N/A" AT 1026
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
	       DISPLAY "N/A" AT 1526
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
	       DISPLAY "N/A" AT 1626
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
	   END-IF.
	   IF LS0-LEV < 3
	       DISPLAY ". N/A" AT 1323
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
	       DISPLAY ". N/A" AT 1724
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
	       DISPLAY " N/A" AT 1825
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
	       DISPLAY ". N/A" AT 1923
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
	   END-IF.
	   IF LS0-DBLEV < 2
	       DISPLAY ". N/A" AT 1923
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
	   END-IF.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.

       BA02.
	     MOVE 5		 TO TOP-ROW.
	     MOVE 20		 TO BOTTOM-ROW.
	     MOVE 4		 TO SCREEN-LIN.
	     MOVE 9		 TO SCREEN-COL.
	     MOVE 19		 TO STRING-LENGTH.
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
		  (WS-OPTION < "A" OR > "G") AND
		  (WS-OPTION NOT = "T")
		   CALL X"E5"
		   GO TO BA05
	       END-IF
	       GO TO BA16
	   ELSE
	       CALL X"E5"
	       GO TO BA05
	   END-IF.
	   IF WS-OPTION = "T"
	       GO TO BA18
	   END-IF.

       BA15.
	     EVALUATE SCREEN-LIN
	       WHEN 5
		   MOVE "1"	 TO WS-OPTION
	       WHEN 6
		   MOVE "2"	 TO WS-OPTION
	       WHEN 7
		   MOVE "3"	 TO WS-OPTION
	       WHEN 8
		   MOVE "4"	 TO WS-OPTION
	       WHEN 9
		   MOVE "5"	 TO WS-OPTION
	       WHEN 10
		   MOVE "6"	 TO WS-OPTION
	       WHEN 11
		   MOVE "7"	 TO WS-OPTION
	       WHEN 12
		   MOVE "8"	 TO WS-OPTION
	       WHEN 13
		   MOVE "A"	 TO WS-OPTION
	       WHEN 14
		   MOVE "B"	 TO WS-OPTION
	       WHEN 15
		   MOVE "C"	 TO WS-OPTION
	       WHEN 16
		   MOVE "D"	 TO WS-OPTION
	       WHEN 17
		   MOVE "E"	 TO WS-OPTION
	       WHEN 18
		   MOVE "F"	 TO WS-OPTION
	       WHEN 19
		   MOVE "G"	 TO WS-OPTION
	       WHEN 20
		   MOVE "0"	 TO WS-OPTION
	       WHEN OTHER
		   CALL X"E5" CALL X"E5" CALL X"E5"
		   GO TO BA05
	     END-EVALUATE.
	     GO TO BA18.

       BA16.
	     PERFORM CLEAR-BLOCK.
	     EVALUATE WS-OPTION
	       WHEN "0"
		   MOVE 20	 TO SCREEN-LIN
	       WHEN "1"
		   MOVE 5	 TO SCREEN-LIN
	       WHEN "2"
		   MOVE 6	 TO SCREEN-LIN
	       WHEN "3"
		   MOVE 7	 TO SCREEN-LIN
	       WHEN "4"
		   MOVE 8	 TO SCREEN-LIN
	       WHEN "5"
		   MOVE 9	 TO SCREEN-LIN
	       WHEN "6"
		   MOVE 10	 TO SCREEN-LIN
	       WHEN "7"
		   MOVE 11	 TO SCREEN-LIN
	       WHEN "8"
		   MOVE 12	 TO SCREEN-LIN
	       WHEN "A"
		   MOVE 13	 TO SCREEN-LIN
	       WHEN "B"
		   MOVE 14	 TO SCREEN-LIN
	       WHEN "C"
		   MOVE 15	 TO SCREEN-LIN
	       WHEN "D"
		   MOVE 16	 TO SCREEN-LIN
	       WHEN "E"
		   MOVE 17	 TO SCREEN-LIN
	       WHEN "F"
		   MOVE 18	 TO SCREEN-LIN
	       WHEN "G"
		   MOVE 19	 TO SCREEN-LIN
	     END-EVALUATE.
	     PERFORM SAVE-ATTR.
	     PERFORM MARK-BLOCK.

       BA18.
	     PERFORM SAVE-SCREEN.
	     MOVE BLOCK-DETAIL	 TO BLOCK-DETAIL1.
	     MOVE SHADOW-DETAIL  TO SHADOW-DETAIL1.
	     MOVE CRT-DETAIL	 TO CRT-DETAIL1.
	     EVALUATE WS-OPTION
	       WHEN "0"
		   GO TO BA999
	       WHEN "1"
		   PERFORM CA000
	       WHEN "2"
		   PERFORM DA000
	       WHEN "3"
		   PERFORM EA000
	       WHEN "4"
		   PERFORM FA000
	       WHEN "5"
		   PERFORM GA000
	       WHEN "6"
		   PERFORM HA000
	       WHEN "7"
		   PERFORM IA000
	       WHEN "8"
		   PERFORM JA000
	       WHEN "A"
		   PERFORM KA000
	       WHEN "B"
		   PERFORM LA000
	       WHEN "C"
		   PERFORM MA000
	       WHEN "D"
		   PERFORM NA000
	       WHEN "E"
		   PERFORM OA000
	       WHEN "F"
		   PERFORM PA000
	       WHEN "G"
		   PERFORM QA000
	     END-EVALUATE.
	     PERFORM RESTORE-SCREEN.
	     MOVE BLOCK-DETAIL1  TO BLOCK-DETAIL.
	     MOVE SHADOW-DETAIL1 TO SHADOW-DETAIL.
	     MOVE CRT-DETAIL1	 TO CRT-DETAIL.
	     GO TO BA05.
       BA999.
	     EXIT.

      /
      *    ****    D E B T O R	 M A I N T E N A N C E
      *
       CA000	       SECTION 50.
       CA00.
	   IF LS-DSKDRV = "L"
	       MOVE "Not available on Local drive"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO CA999
	   END-IF.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 28		 TO SHADE-COL.
	     MOVE 23		 TO SHADE-WIDTH.
	     MOVE 14		 TO SHADE-LINES.
	     DISPLAY S03.
	   IF LS0-LEV < 3
	       DISPLAY " No. (N/A)" AT 1139
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 3
	       DISPLAY "(N/A)" AT 1243
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 3
	       DISPLAY "N/A" AT 1447
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 3
	   END-IF.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.
	     MOVE 7		 TO TOP-ROW.
	     MOVE 19		 TO BOTTOM-ROW.
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
	       IF WS-OPTION < 0 OR > 8 AND
		  NOT (WS-OPTION = "A" OR "C")
		   CALL X"E5"
		   GO TO CA05
	       END-IF
	       GO TO CA16
	   ELSE
	       CALL X"E5"
	       GO TO CA05
	   END-IF.

       CA15.
	     EVALUATE SCREEN-LIN
	       WHEN 7
		   MOVE "1"	 TO WS-OPTION
	       WHEN 8
		   MOVE "2"	 TO WS-OPTION
	       WHEN 9
		   MOVE "3"	 TO WS-OPTION
	       WHEN 10
		   MOVE "4"	 TO WS-OPTION
	       WHEN 11
		   MOVE "5"	 TO WS-OPTION
	       WHEN 12
		   MOVE "6"	 TO WS-OPTION
	       WHEN 13
		   MOVE "7"	 TO WS-OPTION
	       WHEN 14
		   MOVE "8"	 TO WS-OPTION
	       WHEN 15
		   MOVE "A"	 TO WS-OPTION
	       WHEN 16
		   MOVE "B"	 TO WS-OPTION
	       WHEN 17
		   MOVE "C"	 TO WS-OPTION
	       WHEN 18
		   MOVE "0"	 TO WS-OPTION
	       WHEN OTHER
		   CALL X"E5" CALL X"E5" CALL X"E5"
		   GO TO CA05
	     END-EVALUATE.
	     GO TO CA18.

       CA16.
	     PERFORM CLEAR-BLOCK.
	     EVALUATE WS-OPTION
	       WHEN "0"
		   MOVE 18	 TO SCREEN-LIN
	       WHEN "1"
		   MOVE 7	 TO SCREEN-LIN
	       WHEN "2"
		   MOVE 8	 TO SCREEN-LIN
	       WHEN "3"
		   MOVE 9	 TO SCREEN-LIN
	       WHEN "4"
		   MOVE 10	 TO SCREEN-LIN
	       WHEN "5"
		   MOVE 11	 TO SCREEN-LIN
	       WHEN "6"
		   MOVE 12	 TO SCREEN-LIN
	       WHEN "7"
		   MOVE 13	 TO SCREEN-LIN
	       WHEN "8"
		   MOVE 14	 TO SCREEN-LIN
	       WHEN "A"
		   MOVE 15	 TO SCREEN-LIN
	       WHEN "B"
		   MOVE 16	 TO SCREEN-LIN
	       WHEN "C"
		   MOVE 17	 TO SCREEN-LIN
	     END-EVALUATE.
	     PERFORM SAVE-ATTR.
	     PERFORM MARK-BLOCK.

       CA18.
	     EVALUATE WS-OPTION
	       WHEN "0"
		   GO TO CA999
	       WHEN "1"
		   MOVE "DTP\DTP001" TO PRG-NAME
	       WHEN "2"
		   MOVE "DTP\DTP002" TO PRG-NAME
	       WHEN "3"
		   MOVE "DTP\DTP003" TO PRG-NAME
	       WHEN "4"
		   IF LS0-LEV < 3
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		   Not available in this version	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
		       MOVE "MNU\NOPMENU"  TO PRG-NAME
		   ELSE
		       MOVE "DTP\DTP004"   TO PRG-NAME
		   END-IF
	       WHEN "5"
		   IF LS0-LEV < 3
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		   Not available in this version	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
		       MOVE "MNU\NOPMENU"  TO PRG-NAME
		   ELSE
		       MOVE "DTP\DTP005"   TO PRG-NAME
		   END-IF
	       WHEN "6"
		   MOVE "DTP\DTP006" TO PRG-NAME
	       WHEN "7"
		   IF LS0-LEV < 3
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		   Not available in this version	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
		       MOVE "MNU\NOPMENU"  TO PRG-NAME
		   ELSE
		       MOVE "DTP\DTP007"   TO PRG-NAME
		   END-IF
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³			Sales Restrictions		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	       WHEN "8"
		  MOVE "DTP\DTPDPT"  TO PRG-NAME
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³			     Enquiries			       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	       WHEN "A"
		   MOVE "DTP\DTPENQ" TO PRG-NAME
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³			 Enquiries (History)		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	       WHEN "B"
		   MOVE "DTP\DTPENH" TO PRG-NAME
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³			 Balance Take-on		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	       WHEN "C"
		   MOVE "DTP\DTP099" TO PRG-NAME
	     END-EVALUATE.
	     PERFORM AA100.
	     DISPLAY MENU-INSTRUCT.
	     GO TO CA05.
       CA999.
	     EXIT.
      /
      *    ****    D E B T O R	 T R A N S A C T I O N S
      *
       DA000	       SECTION 51.
       DA00.
	   IF LS-DSKDRV = "L"
	       MOVE "Not available on Local drive"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO DA999
	   END-IF.
003240	   IF LS0-DBLEV < 2
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO DA999
	   END-IF.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 28		 TO SHADE-COL.
	     MOVE 22		 TO SHADE-WIDTH.
	     MOVE 13		 TO SHADE-LINES.
	     DISPLAY S04.
	   IF LS0-LEV < 3
	       DISPLAY "N/A" AT 1245
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 3
	       DISPLAY "N/A" AT 1545
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 3
	   END-IF.
	   IF LS0-LEV < 4
	       DISPLAY ". N/A" AT 1644
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 3
	   END-IF.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.
	     MOVE 7		 TO TOP-ROW.
	     MOVE 17		 TO BOTTOM-ROW.
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
	       IF WS-OPTION < 0 OR > 8 AND
		  NOT (WS-OPTION = "A" OR "B")
		   CALL X"E5"
		   GO TO DA05
	       END-IF
	       GO TO DA16
	   ELSE
	       CALL X"E5"
	       GO TO DA05
	   END-IF.

       DA15.
	     EVALUATE SCREEN-LIN
	       WHEN 7
		   MOVE "1"	 TO WS-OPTION
	       WHEN 8
		   MOVE "2"	 TO WS-OPTION
	       WHEN 9
		   MOVE "3"	 TO WS-OPTION
	       WHEN 10
		   MOVE "4"	 TO WS-OPTION
	       WHEN 11
		   MOVE "5"	 TO WS-OPTION
	       WHEN 12
		   MOVE "6"	 TO WS-OPTION
	       WHEN 13
		   MOVE "7"	 TO WS-OPTION
	       WHEN 14
		   MOVE "8"	 TO WS-OPTION
	       WHEN 15
		   MOVE "A"	 TO WS-OPTION
	       WHEN 16
		   MOVE "B"	 TO WS-OPTION
	       WHEN 17
		   MOVE "0"	 TO WS-OPTION
	       WHEN OTHER
		   CALL X"E5" CALL X"E5" CALL X"E5"
		   GO TO DA05
	     END-EVALUATE.
	   GO TO DA18.

       DA16.
	     PERFORM CLEAR-BLOCK.
	     EVALUATE WS-OPTION
	       WHEN "0"
		   MOVE 17	 TO SCREEN-LIN
	       WHEN "1"
		   MOVE 7	 TO SCREEN-LIN
	       WHEN "2"
		   MOVE 8	 TO SCREEN-LIN
	       WHEN "3"
		   MOVE 9	 TO SCREEN-LIN
	       WHEN "4"
		   MOVE 10	 TO SCREEN-LIN
	       WHEN "5"
		   MOVE 11	 TO SCREEN-LIN
	       WHEN "6"
		   MOVE 12	 TO SCREEN-LIN
	       WHEN "7"
		   MOVE 13	 TO SCREEN-LIN
	       WHEN "8"
		   MOVE 14	 TO SCREEN-LIN
	       WHEN "A"
		   MOVE 15	 TO SCREEN-LIN
	       WHEN "B"
		   MOVE 16	 TO SCREEN-LIN
	     END-EVALUATE.
	     PERFORM SAVE-ATTR.
	     PERFORM MARK-BLOCK.

       DA18.
	     EVALUATE WS-OPTION
	       WHEN "0"
		   GO TO DA999
	       WHEN "1"
		   MOVE "DTP\DTP008" TO PRG-NAME
	       WHEN "2"
		   MOVE "DTP\DTP009" TO PRG-NAME
	       WHEN "3"
		   MOVE "DTP\DTP010" TO PRG-NAME
	       WHEN "4"
		   MOVE "DTP\DTP011" TO PRG-NAME
	       WHEN "5"
		   IF LS0-LEV < 3
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		   Not available in this version	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
		       MOVE "MNU\NOPMENU"  TO PRG-NAME
		   ELSE
		       MOVE "DTP\DTP012"   TO PRG-NAME
		   END-IF
	       WHEN "6"
		   MOVE "DTP\DTP013" TO PRG-NAME
	       WHEN "7"
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³			     Enquiries			       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
		   MOVE "DTP\DTPENQ" TO PRG-NAME
	       WHEN "8"
		   IF LS0-LEV < 3
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		   Not available in this version	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
		       MOVE "MNU\NOPMENU"  TO PRG-NAME
		   ELSE
		       MOVE "DTP\DTPSAL"   TO PRG-NAME
		   END-IF
	       WHEN "A"
		   MOVE "DTP\DTP015" TO PRG-NAME
	       WHEN "B"
		   IF LS0-LEV < 4
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		   Not available in this version	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
		       MOVE "MNU\NOPMENU"  TO PRG-NAME
		   ELSE
		       MOVE "DTP\DTP014"   TO PRG-NAME
		   END-IF
	     END-EVALUATE.
	     PERFORM AA100.
	     DISPLAY MENU-INSTRUCT.
	     GO TO DA05.

       DA999.
	     EXIT.
      /
      *    ****   D E B T O R	R E P O R T S
      *
       EA000	       SECTION 52.
       EA00.
003240	   IF LS0-DBLEV < 2
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO EA999
	   END-IF.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 28		 TO SHADE-COL.
	     MOVE 23		 TO SHADE-WIDTH.
	     MOVE 6		 TO SHADE-LINES.
	     DISPLAY S05.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.
	     MOVE 7		 TO TOP-ROW.
	     MOVE 10		 TO BOTTOM-ROW.
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
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
	       IF (WS-OPTION < 0 OR > 3)
		   CALL X"E5"
		   GO TO EA05
	       END-IF
	       GO TO EA16
	   ELSE
	       CALL X"E5"
	       GO TO EA05
	   END-IF.

       EA15.
	     EVALUATE SCREEN-LIN
	       WHEN 7
		   MOVE "1"	 TO WS-OPTION
	       WHEN 8
		   MOVE "2"	 TO WS-OPTION
	       WHEN 9
		   MOVE "3"	 TO WS-OPTION
	       WHEN 10
		   MOVE "0"	 TO WS-OPTION
	       WHEN OTHER
		   CALL X"E5" CALL X"E5" CALL X"E5"
		   GO TO EA05
	     END-EVALUATE.
	     GO TO EA18.

       EA16.
	     PERFORM CLEAR-BLOCK.
	     EVALUATE WS-OPTION
	       WHEN "0"
		   MOVE 10	 TO SCREEN-LIN
	       WHEN "1"
		   MOVE 7	 TO SCREEN-LIN
	       WHEN "2"
		   MOVE 8	 TO SCREEN-LIN
	       WHEN "3"
		   MOVE 9	 TO SCREEN-LIN
	     END-EVALUATE.
	     PERFORM SAVE-ATTR.
	     PERFORM MARK-BLOCK.

       EA18.
	     PERFORM SAVE-SCREEN-2.
	     MOVE BLOCK-DETAIL	 TO BLOCK-DETAIL2.
	     MOVE SHADOW-DETAIL  TO SHADOW-DETAIL2.
	     MOVE CRT-DETAIL	 TO CRT-DETAIL2.
	     EVALUATE WS-OPTION
	       WHEN "0"
		   GO TO EA999
	       WHEN "1"
		   PERFORM EB000
	       WHEN "2"
		   PERFORM EC000
	       WHEN "3"
		   PERFORM ED000
	     END-EVALUATE.
	     PERFORM RESTORE-SCREEN-2.
	     MOVE BLOCK-DETAIL2  TO BLOCK-DETAIL.
	     MOVE SHADOW-DETAIL2 TO SHADOW-DETAIL.
	     MOVE CRT-DETAIL2	 TO CRT-DETAIL.
	     GO TO EA05.

       EA999.
	     EXIT.
      /
      *    ****   D E B T O R	P E R I O D - E N D  R E P O R T S
      *
       EB000	       SECTION 52.
       EB00.
	     MOVE 9		 TO SHADE-ROW.
	     MOVE 50		 TO SHADE-COL.
	     MOVE 23		 TO SHADE-WIDTH.
	     MOVE 10		 TO SHADE-LINES.
	     DISPLAY S06A.
	   IF LS0-LEV < 3
      *
      *     Detailed Accounts
      *
	       DISPLAY "nts N/A" AT 1163
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
	   END-IF.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.
	     MOVE 9		 TO TOP-ROW.
	     MOVE 17		 TO BOTTOM-ROW.
	     MOVE 8		 TO SCREEN-LIN.
	     MOVE 50		 TO SCREEN-COL.
	     MOVE 21		 TO STRING-LENGTH.
	     PERFORM SAVE-ATTR.

       EB05.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       EB10.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY	 PERFORM MOVE-BLOCK-UP
		 WHEN DOWN-KEY	 PERFORM MOVE-BLOCK-DOWN
		 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
				 IF MOUSE-ENTER = "Y"
				     MOVE "N"	 TO MOUSE-ENTER
				     GO TO EB15
				 END-IF
				 IF MOUSE-Y = 24
				     IF MOUSE-X = 5
					 MOVE 5  TO KEY-CODE-1
					 GO TO EB10
				     ELSE
				     IF MOUSE-X = 10
					 MOVE 6  TO KEY-CODE-1
					 GO TO EB10
				     ELSE
				     IF MOUSE-X > 36 AND < 44
					 MOVE 0  TO KEY-CODE-1
					 GO TO EB10
				     ELSE
				     IF MOUSE-X > 54 AND < 60
					 MOVE 0  TO KEY-CODE-1
					 MOVE 1  TO KEY-TYPE
					 GO TO EB10
				     END-IF
				 END-IF
		 WHEN ENTER-KEY  GO TO EB15
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO EB05
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE "0"	 TO WS-OPTION
		     GO TO EB18
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO EB05
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
	       IF (WS-OPTION < 0 OR > 7)
		   CALL X"E5"
		   GO TO EB05
	       END-IF
	       GO TO EB16
	   ELSE
	       CALL X"E5"
	       GO TO EB05
	   END-IF.

       EB15.
	     EVALUATE SCREEN-LIN
	       WHEN 9
		   MOVE "1"	 TO WS-OPTION
	       WHEN 10
		   MOVE "2"	 TO WS-OPTION
	       WHEN 11
		   MOVE "3"	 TO WS-OPTION
	       WHEN 12
		   MOVE "4"	 TO WS-OPTION
	       WHEN 13
		   MOVE "5"	 TO WS-OPTION
	       WHEN 14
		   MOVE "6"	 TO WS-OPTION
	       WHEN 15
		   MOVE "7"	 TO WS-OPTION
	       WHEN 16
		   MOVE "0"	 TO WS-OPTION
	       WHEN OTHER
		   CALL X"E5" CALL X"E5" CALL X"E5"
		   GO TO EB05
	     END-EVALUATE.
	     GO TO EB18.

       EB16.
	     PERFORM CLEAR-BLOCK.
	     EVALUATE WS-OPTION
	       WHEN "0"
		   MOVE 16	 TO SCREEN-LIN
	       WHEN "1"
		   MOVE 9	 TO SCREEN-LIN
	       WHEN "2"
		   MOVE 10	 TO SCREEN-LIN
	       WHEN "3"
		   MOVE 11	 TO SCREEN-LIN
	       WHEN "4"
		   MOVE 12	 TO SCREEN-LIN
	       WHEN "5"
		   MOVE 13	 TO SCREEN-LIN
	       WHEN "6"
		   MOVE 14	 TO SCREEN-LIN
	       WHEN "7"
		   MOVE 15	 TO SCREEN-LIN
	     END-EVALUATE.
	     PERFORM SAVE-ATTR.
	     PERFORM MARK-BLOCK.

       EB18.
	     EVALUATE WS-OPTION
	       WHEN "0"
		   GO TO EB999
	       WHEN "1"
		   MOVE "DTP\DTP026" TO PRG-NAME
	       WHEN "2"
		   IF LS0-LEV < 3
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		   Not available in this version	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
		       MOVE "MNU\NOPMENU"  TO PRG-NAME
		   ELSE
		       MOVE "DTP\DTP030"   TO PRG-NAME
		   END-IF
	       WHEN "3"
		   MOVE PRG-STATEMENT
				     TO PRG-NAME
	       WHEN "4"
		   MOVE "DTP\DTP025" TO PRG-NAME
	       WHEN "5"
		   MOVE "DTP\DTP020" TO PRG-NAME
	       WHEN "6"
		   MOVE "DTP\DTP021" TO PRG-NAME
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	 USB and Windows controlled printers - Statements      ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	       WHEN "7"
		   MOVE PRG-WSTATEMENT
				     TO PRG-NAME
	     END-EVALUATE.
	     PERFORM AA100.
	     DISPLAY MENU-INSTRUCT.
	     GO TO EB05.

       EB999.
	     EXIT.
      /
      *    ****   D E B T O R	S T A T I S T I C A L	R E P O R T S
      *
       EC000	       SECTION 52.
       EC00.
	     MOVE 9		 TO SHADE-ROW.
	     MOVE 50		 TO SHADE-COL.
	     MOVE 24		 TO SHADE-WIDTH.
	     MOVE 9		 TO SHADE-LINES.
	     DISPLAY S06B.
	   IF LS0-LEV < 3
      *
      *     Accounts per Rep.
      *
	       DISPLAY "N/A" AT 1070
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
      *
      *     G.P. per Account
      *
	       DISPLAY "N/A" AT 1270
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
      *
      *	    Purchase Statistics
      *
	       DISPLAY "s N/A " AT 1366
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	   IF LS0-LEV < 2
      *
      *     Sales per Account
      *
	       DISPLAY "N/A" AT 1470
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	   IF LS0-LEV < 3
      *
      *     Statistics per Account/Department
      *
	       DISPLAY " N/A" AT 1569
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.
	     MOVE 9		 TO TOP-ROW.
	     MOVE 15		 TO BOTTOM-ROW.
	     MOVE 8		 TO SCREEN-LIN.
	     MOVE 50		 TO SCREEN-COL.
	     MOVE 22		 TO STRING-LENGTH.
	     PERFORM SAVE-ATTR.

       EC05.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       EC10.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY	 PERFORM MOVE-BLOCK-UP
		 WHEN DOWN-KEY	 PERFORM MOVE-BLOCK-DOWN
		 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
				 IF MOUSE-ENTER = "Y"
				     MOVE "N"	 TO MOUSE-ENTER
				     GO TO EC15
				 END-IF
				 IF MOUSE-Y = 24
				     IF MOUSE-X = 5
					 MOVE 5  TO KEY-CODE-1
					 GO TO EC10
				     ELSE
				     IF MOUSE-X = 10
					 MOVE 6  TO KEY-CODE-1
					 GO TO EC10
				     ELSE
				     IF MOUSE-X > 36 AND < 44
					 MOVE 0  TO KEY-CODE-1
					 GO TO EC10
				     ELSE
				     IF MOUSE-X > 54 AND < 60
					 MOVE 0  TO KEY-CODE-1
					 MOVE 1  TO KEY-TYPE
					 GO TO EC10
				     END-IF
				 END-IF
		 WHEN ENTER-KEY  GO TO EC15
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO EC05
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE "0"	 TO WS-OPTION
		     GO TO EC18
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO EC05
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
	       IF (WS-OPTION < 0 OR > 6)
		   CALL X"E5"
		   GO TO EC05
	       END-IF
	       GO TO EC16
	   ELSE
	       CALL X"E5"
	       GO TO EC05.

       EC15.
	     EVALUATE SCREEN-LIN
	       WHEN 9
		   MOVE "1"	 TO WS-OPTION
	       WHEN 10
		   MOVE "2"	 TO WS-OPTION
	       WHEN 11
		   MOVE "3"	 TO WS-OPTION
	       WHEN 12
		   MOVE "4"	 TO WS-OPTION
	       WHEN 13
		   MOVE "5"	 TO WS-OPTION
	       WHEN 14
		   MOVE "6"	 TO WS-OPTION
	       WHEN 15
		   MOVE "0"	 TO WS-OPTION
	       WHEN OTHER
		   CALL X"E5" CALL X"E5" CALL X"E5"
		   GO TO EC05
	     END-EVALUATE.
	     GO TO EC18.

       EC16.
	     PERFORM CLEAR-BLOCK.
	     EVALUATE WS-OPTION
	       WHEN "0"
		   MOVE 15	 TO SCREEN-LIN
	       WHEN "1"
		   MOVE 9	 TO SCREEN-LIN
	       WHEN "2"
		   MOVE 10	 TO SCREEN-LIN
	       WHEN "3"
		   MOVE 11	 TO SCREEN-LIN
	       WHEN "4"
		   MOVE 12	 TO SCREEN-LIN
	       WHEN "5"
		   MOVE 13	 TO SCREEN-LIN
	       WHEN "6"
		   MOVE 14	 TO SCREEN-LIN
	     END-EVALUATE.
	     PERFORM SAVE-ATTR.
	     PERFORM MARK-BLOCK.

       EC18.
	     EVALUATE WS-OPTION
	       WHEN "0"
		   GO TO EC999
	       WHEN "1"
		   IF LS0-LEV < 3
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		   Not available in this version	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
		       MOVE "MNU\NOPMENU"  TO PRG-NAME
		   ELSE
		       MOVE "DTP\DTPREP"   TO PRG-NAME
		   END-IF
	       WHEN "2"
		   MOVE "DTP\DTP024" TO PRG-NAME
	       WHEN "3"
		   IF LS0-LEV < 3
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		   Not available in this version	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
		       MOVE "MNU\NOPMENU"  TO PRG-NAME
		   ELSE
		       MOVE "DTP\DTPPROF"  TO PRG-NAME
		   END-IF
	       WHEN "4"
		   IF LS0-LEV < 3
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		   Not available in this version	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
		       MOVE "MNU\NOPMENU"  TO PRG-NAME
		   ELSE
		       MOVE "DTP\DTP028"   TO PRG-NAME
		   END-IF
	       WHEN "5"
		   IF LS0-LEV < 2
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		   Not available in this version	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
		       MOVE "MNU\NOPMENU"  TO PRG-NAME
		   ELSE
		       MOVE "DTP\DTP029"   TO PRG-NAME
		   END-IF
	       WHEN "6"
		   IF LS0-LEV < 3
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		   Not available in this version	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
		       MOVE "MNU\NOPMENU"  TO PRG-NAME
		   ELSE
		       MOVE "DTP\DTPSLD"  TO PRG-NAME
		   END-IF
	     END-EVALUATE.
	     PERFORM AA100.
	     DISPLAY MENU-INSTRUCT.
	     GO TO EC05.

       EC999.
	     EXIT.
      /
      *    ****   D E B T O R	S U N D R Y   R E P O R T S
      *
       ED000	       SECTION 52.
       ED0.
003240	   IF LS0-DBLEV < 3
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO ED999.

       ED00.
	     MOVE 9		 TO SHADE-ROW.
	     MOVE 50		 TO SHADE-COL.
	     MOVE 24		 TO SHADE-WIDTH.
	     MOVE 8		 TO SHADE-LINES.
	     DISPLAY S06C.
	   IF LS0-LEV < 2
      *
      *     Account/Discount Structure
      *
	       DISPLAY "N/A" AT 1069
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.
	     MOVE 9		 TO TOP-ROW.
	     MOVE 14		 TO BOTTOM-ROW.
	     MOVE 8		 TO SCREEN-LIN.
	     MOVE 50		 TO SCREEN-COL.
	     MOVE 22		 TO STRING-LENGTH.
	     PERFORM SAVE-ATTR.

       ED05.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       ED10.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY	 PERFORM MOVE-BLOCK-UP
		 WHEN DOWN-KEY	 PERFORM MOVE-BLOCK-DOWN
		 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
				 IF MOUSE-ENTER = "Y"
				     MOVE "N"	 TO MOUSE-ENTER
				     GO TO ED15
				 END-IF
				 IF MOUSE-Y = 24
				     IF MOUSE-X = 5
					 MOVE 5  TO KEY-CODE-1
					 GO TO ED10
				     ELSE
				     IF MOUSE-X = 10
					 MOVE 6  TO KEY-CODE-1
					 GO TO ED10
				     ELSE
				     IF MOUSE-X > 36 AND < 44
					 MOVE 0  TO KEY-CODE-1
					 GO TO ED10
				     ELSE
				     IF MOUSE-X > 54 AND < 60
					 MOVE 0  TO KEY-CODE-1
					 MOVE 1  TO KEY-TYPE
					 GO TO ED10
				     END-IF
				 END-IF
		 WHEN ENTER-KEY  GO TO ED15
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO ED05
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE "0"	 TO WS-OPTION
		     GO TO ED18
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO ED05
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
	       IF (WS-OPTION < 0 OR > 5)
		   CALL X"E5"
		   GO TO ED05
	       END-IF
	       GO TO ED16
	   ELSE
	       CALL X"E5"
	       GO TO ED05.

       ED15.
	     EVALUATE SCREEN-LIN
	       WHEN 9
		   MOVE "1"	 TO WS-OPTION
	       WHEN 10
		   MOVE "2"	 TO WS-OPTION
	       WHEN 11
		   MOVE "3"	 TO WS-OPTION
	       WHEN 12
		   MOVE "4"	 TO WS-OPTION
	       WHEN 13
		   MOVE "5"	 TO WS-OPTION
	       WHEN 14
		   MOVE "0"	 TO WS-OPTION
	       WHEN OTHER
		   CALL X"E5" CALL X"E5" CALL X"E5"
		   GO TO ED05
	     END-EVALUATE.
	     GO TO ED18.

       ED16.
	     PERFORM CLEAR-BLOCK.
	     EVALUATE WS-OPTION
	       WHEN "0"
		   MOVE 14	 TO SCREEN-LIN
	       WHEN "1"
		   MOVE 9	 TO SCREEN-LIN
	       WHEN "2"
		   MOVE 10	 TO SCREEN-LIN
	       WHEN "3"
		   MOVE 11	 TO SCREEN-LIN
	       WHEN "4"
		   MOVE 12	 TO SCREEN-LIN
	       WHEN "5"
		   MOVE 13	 TO SCREEN-LIN
	     END-EVALUATE.
	     PERFORM SAVE-ATTR.
	     PERFORM MARK-BLOCK.

       ED18.
	     EVALUATE WS-OPTION
	       WHEN "0"
		   GO TO ED999
	       WHEN "1"
		   IF LS0-LEV < 2
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		   Not available in this version	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
		       MOVE "MNU\NOPMENU"  TO PRG-NAME
		   ELSE
		       MOVE "DTP\DTPDIS"   TO PRG-NAME
		   END-IF
	       WHEN "2"
		   MOVE "DTP\DTP023" TO PRG-NAME
	       WHEN "3"
		   MOVE "DTP\DTP027" TO PRG-NAME
	       WHEN "4"
		   MOVE "DTP\DTP022" TO PRG-NAME
	       WHEN "5"
		   MOVE "DTP\DTP021" TO PRG-NAME
	     END-EVALUATE.
	     PERFORM AA100.
	     DISPLAY MENU-INSTRUCT.
	     GO TO ED05.

       ED999.
	     EXIT.
      /
      *    ****   D E B T O R	C O L E C T I O N   E N Q U I R Y
      *
       FA000	       SECTION 53.
       FA00.
	   IF LS0-LEV < 3
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		   Not available in this version	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	       MOVE "MNU\NOPMENU" TO PRG-NAME
	   ELSE
	       MOVE "DTP\DTPCOL"  TO PRG-NAME
	   END-IF.
	     PERFORM AA100.

       FA999.
	     EXIT.
      /
      *    ****    D E B T O R	 E X P O R T S
      *
       GA000	       SECTION 54.
       GA00.
	   IF LS-DSKDRV = "L"
	       MOVE "Not available on Local drive"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO GA999
	   END-IF.
	   IF LS0-LEV < 2
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		   Not available in this version	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	       MOVE "MNU\NOPMENU"
				 TO PRG-NAME
	   ELSE
	       MOVE "DTP\DTPEXP" TO PRG-NAME
	   END-IF.
	     PERFORM AA100.

       GA999.
	     EXIT.
      /
      *    ****   L O O K U P	R E - P R I N T   I N V O I C E
      *
       HA000	       SECTION 55.
       HA00.
	   IF LS-DSKDRV = "L"
	       MOVE "Not available on Local drive"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO HA999
	   END-IF.
	     OPEN I-O STOCK.
	     MOVE "INV\INVDUP"	 TO PRG-NAME.
	     PERFORM AA100.
	     CLOSE STOCK.

       HA999.
	     EXIT.
      /
      *    ****   D E B T O R	U T I L I T I E S
      *
       IA000	       SECTION 56.
       IA00.
	   IF LS-DSKDRV = "L"
	       MOVE "Not available on Local drive"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO IA999
	   END-IF.
003240	   IF LS0-DBLEV < 3
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO IA999
	   END-IF.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 28		 TO SHADE-COL.
	     MOVE 23		 TO SHADE-WIDTH.
	     MOVE 13		 TO SHADE-LINES.
	     DISPLAY S07.
	   IF LS0-LEV < 3
	       DISPLAY "(N/A)" AT 0845
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 3
	       DISPLAY "N/A" AT 1447
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 3
	   END-IF.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.
	     MOVE 7		 TO TOP-ROW.
	     MOVE 17		 TO BOTTOM-ROW.
	     MOVE 6		 TO SCREEN-LIN.
	     MOVE 28		 TO SCREEN-COL.
	     MOVE 21		 TO STRING-LENGTH.
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
	       IF (WS-OPTION < 0 OR > 8) AND
		  (WS-OPTION NOT = "A") AND
		  (WS-OPTION NOT = "B") AND
		  (WS-OPTION NOT = "@") AND
		  (WS-OPTION NOT = "&") AND
		  (WS-OPTION NOT = "$") AND
		  (WS-OPTION NOT = "#") AND
		  (WS-OPTION NOT = "!") AND
		  (WS-OPTION NOT = "%")
		   CALL X"E5"
		   GO TO IA05
	       END-IF
	       GO TO IA16
	   ELSE
	       CALL X"E5"
	       GO TO IA05
	   END-IF.

       IA15.
	     EVALUATE SCREEN-LIN
	       WHEN 7
		   MOVE "1"	 TO WS-OPTION
	       WHEN 8
		   MOVE "2"	 TO WS-OPTION
	       WHEN 9
		   MOVE "3"	 TO WS-OPTION
	       WHEN 10
		   MOVE "4"	 TO WS-OPTION
	       WHEN 11
		   MOVE "5"	 TO WS-OPTION
	       WHEN 12
		   MOVE "6"	 TO WS-OPTION
	       WHEN 13
		   MOVE "7"	 TO WS-OPTION
	       WHEN 14
		   MOVE "8"	 TO WS-OPTION
	       WHEN 15
		   MOVE "A"	 TO WS-OPTION
	       WHEN 16
		   MOVE "B"	 TO WS-OPTION
	       WHEN 17
		   MOVE "0"	 TO WS-OPTION
	       WHEN OTHER
		   CALL X"E5" CALL X"E5" CALL X"E5"
		   GO TO IA05
	     END-EVALUATE.
	     GO TO IA18.

       IA16.
	     PERFORM CLEAR-BLOCK.
	     EVALUATE WS-OPTION
	       WHEN "0"
		   MOVE 17	 TO SCREEN-LIN
	       WHEN "1"
		   MOVE 7	 TO SCREEN-LIN
	       WHEN "2"
		   MOVE 8	 TO SCREEN-LIN
	       WHEN "3"
		   MOVE 9	 TO SCREEN-LIN
	       WHEN "4"
		   MOVE 10	 TO SCREEN-LIN
	       WHEN "5"
		   MOVE 11	 TO SCREEN-LIN
	       WHEN "6"
		   MOVE 12	 TO SCREEN-LIN
	       WHEN "7"
		   MOVE 13	 TO SCREEN-LIN
	       WHEN "8"
		   MOVE 14	 TO SCREEN-LIN
	       WHEN "A"
		   MOVE 15	 TO SCREEN-LIN
	       WHEN "B"
		   MOVE 16	 TO SCREEN-LIN
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ S P E C I A L   R O U T I N E S   (UTILITIES Not in Menu) ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	       WHEN "@"
	       WHEN "&"
	       WHEN "$"
	       WHEN "#"
	       WHEN "!"
	       WHEN "%"
		   MOVE 7	 TO SCREEN-LIN
	     END-EVALUATE.
	     PERFORM SAVE-ATTR.
	     PERFORM MARK-BLOCK.

       IA18.
	     EVALUATE WS-OPTION
	       WHEN "0"
		   GO TO IA999
	       WHEN "1"
		   IF LS0-LEV < 3
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		   Not available in this version	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
		       MOVE "MNU\NOPMENU"  TO PRG-NAME
		   ELSE
		       MOVE "DTP\DTP091"   TO PRG-NAME
		   END-IF
	       WHEN "2"
		   MOVE "DTP\DTP092" TO PRG-NAME
	       WHEN "3"
		   MOVE "DTP\DTP093" TO PRG-NAME
	       WHEN "4"
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³			     Enquiries			       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
		   MOVE "DTP\DTPENQ" TO PRG-NAME
	       WHEN "5"
		   MOVE "DTP\DTP095" TO PRG-NAME
	       WHEN "6"
		   MOVE "DTP\DTP096" TO PRG-NAME
	       WHEN "7"
		   IF LS0-LEV < 3
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		   Not available in this version	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
		       MOVE "MNU\NOPMENU"  TO PRG-NAME
		   ELSE
		       MOVE "DTP\DTP090"   TO PRG-NAME
		   END-IF
	       WHEN "8"
		   MOVE "DTP\DTPDTE" TO PRG-NAME
	       WHEN "A"
		   MOVE "DTP\DTPAGE" TO PRG-NAME
	       WHEN "B"
		   MOVE "Y"	     TO WS-REOPEN
		   CLOSE DEBALT
			 DEBTOR
			 NETWORK
			 PARAM
		   MOVE "UTP\DALREB" TO PRG-NAME
	       WHEN "&"
		   MOVE "DTP\DTP097" TO PRG-NAME
	       WHEN "@"
		   MOVE "DTP\DTP098" TO PRG-NAME
	       WHEN "$"
		   MOVE "DTP\DTPMRG" TO PRG-NAME
	       WHEN "#"
		   MOVE "DTP\DTPTCL" TO PRG-NAME
	       WHEN "!"
		   MOVE "DTP\DTPMINV"
				     TO PRG-NAME
	       WHEN "%"
		   MOVE "DTP\DTPPAD"
				     TO PRG-NAME
	     END-EVALUATE.
	     PERFORM AA100.
	   IF WS-REOPEN = "Y"
	       MOVE SPACE	 TO WS-REOPEN
	       OPEN I-O DEBALT
			DEBTOR
			NETWORK
			PARAM.
	     DISPLAY MENU-INSTRUCT.
	     GO TO IA05.

       IA999.
	     EXIT.
      /
      *    ****   D E B T O R	R E N T A L S	/   S E R V I C E
      *
       JA000	       SECTION 57.
       JA00.
	   IF LS-DSKDRV = "L"
	       MOVE "Not available on Local drive"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO JA999
	   END-IF.
	   IF LS0-LEV < 3
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		   Not available in this version	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	       MOVE "MNU\NOPMENU"
				 TO PRG-NAME
	       PERFORM AA100
	       DISPLAY MENU-INSTRUCT
	       GO TO JA999
	   END-IF.
003240	   IF LS0-DBLEV < 3
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO JA999
	   END-IF.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 28		 TO SHADE-COL.
	     MOVE 23		 TO SHADE-WIDTH.
	     MOVE 10		 TO SHADE-LINES.
	     DISPLAY S08.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.
	     MOVE 7		 TO TOP-ROW.
	     MOVE 14		 TO BOTTOM-ROW.
	     MOVE 6		 TO SCREEN-LIN.
	     MOVE 28		 TO SCREEN-COL.
	     MOVE 21		 TO STRING-LENGTH.
	     PERFORM SAVE-ATTR.

       JA05.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       JA10.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY	 PERFORM MOVE-BLOCK-UP
		 WHEN DOWN-KEY	 PERFORM MOVE-BLOCK-DOWN
		 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
				 IF MOUSE-ENTER = "Y"
				     MOVE "N"	 TO MOUSE-ENTER
				     GO TO JA15
				 END-IF
				 IF MOUSE-Y = 24
				     IF MOUSE-X = 5
					 MOVE 5  TO KEY-CODE-1
					 GO TO JA10
				     ELSE
				     IF MOUSE-X = 10
					 MOVE 6  TO KEY-CODE-1
					 GO TO JA10
				     ELSE
				     IF MOUSE-X > 36 AND < 44
					 MOVE 0  TO KEY-CODE-1
					 GO TO JA10
				     ELSE
				     IF MOUSE-X > 54 AND < 60
					 MOVE 0  TO KEY-CODE-1
					 MOVE 1  TO KEY-TYPE
					 GO TO JA10
				     END-IF
				 END-IF
		 WHEN ENTER-KEY  GO TO JA15
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO JA05
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE "0"	 TO WS-OPTION
		     GO TO JA18
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO JA05
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
	       IF WS-OPTION < 0 OR > 7
		   CALL X"E5"
		   GO TO JA05
	       END-IF
	       GO TO JA16
	   ELSE
	       CALL X"E5"
	       GO TO JA05
	   END-IF.

       JA15.
	     EVALUATE SCREEN-LIN
	       WHEN 7
		   MOVE "1"	 TO WS-OPTION
	       WHEN 8
		   MOVE "2"	 TO WS-OPTION
	       WHEN 9
		   MOVE "3"	 TO WS-OPTION
	       WHEN 10
		   MOVE "4"	 TO WS-OPTION
	       WHEN 11
		   MOVE "5"	 TO WS-OPTION
	       WHEN 12
		   MOVE "6"	 TO WS-OPTION
	       WHEN 13
		   MOVE "7"	 TO WS-OPTION
	       WHEN 14
		   MOVE "0"	 TO WS-OPTION
	       WHEN OTHER
		   CALL X"E5" CALL X"E5" CALL X"E5"
		   GO TO JA05
	     END-EVALUATE.
	     GO TO JA18.

       JA16.
	     PERFORM CLEAR-BLOCK.
	     EVALUATE WS-OPTION
	       WHEN "0"
		   MOVE 14	 TO SCREEN-LIN
	       WHEN "1"
		   MOVE 7	 TO SCREEN-LIN
	       WHEN "2"
		   MOVE 8	 TO SCREEN-LIN
	       WHEN "3"
		   MOVE 9	 TO SCREEN-LIN
	       WHEN "4"
		   MOVE 10	 TO SCREEN-LIN
	       WHEN "5"
		   MOVE 11	 TO SCREEN-LIN
	       WHEN "6"
		   MOVE 12	 TO SCREEN-LIN
	       WHEN "7"
		   MOVE 13	 TO SCREEN-LIN
	     END-EVALUATE.
	     PERFORM SAVE-ATTR.
	     PERFORM MARK-BLOCK.

       JA18.
	     EVALUATE WS-OPTION
	       WHEN "0"
		   GO TO JA999
	       WHEN "1"
		   MOVE "DTP\DTP031" TO PRG-NAME
	       WHEN "2"
		   MOVE "DTP\DTP032" TO PRG-NAME
	       WHEN "3"
		   MOVE "DTP\DTP033" TO PRG-NAME
	       WHEN "4"
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³			     Enquiries			       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
		   MOVE "DTP\DTPENQ" TO PRG-NAME
	       WHEN "5"
		   MOVE "DTP\DTP034" TO PRG-NAME
	       WHEN "6"
		   MOVE "DTP\DTP035" TO PRG-NAME
	       WHEN "7"
		   MOVE "DTP\DTP036" TO PRG-NAME
	     END-EVALUATE.
	     PERFORM AA100.
	     DISPLAY MENU-INSTRUCT.
	     GO TO JA05.

       JA999.
	     EXIT.

      /
      *    ****   D E B T O R	E N Q U I R Y
      *
       KA000	       SECTION 58.
       KA00.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³			     Enquiries			       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE "DTP\DTPENQ"	 TO PRG-NAME.
	     PERFORM AA100.

       KA999.
	     EXIT.
      /
      *    ****   D E B T O R	S A L E S   T R A C K I N G
      *
       LA000	       SECTION 58.
       LA00.
	   IF LS0-LEV < 2
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		   Not available in this version	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	       MOVE "MNU\NOPMENU" TO PRG-NAME
	   ELSE
	       MOVE "DTP\DTPSAL"  TO PRG-NAME
	   END-IF.
	     PERFORM AA100.

       LA999.
	     EXIT.
      /
      *    ****   D E B T O R	I M P O R T   O P T I O N S
      *
       MA000	       SECTION 59.
       MA00.
	   IF LS-DSKDRV = "L"
	       MOVE "Not available on Local drive"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO MA999
	   END-IF.
	   IF LS0-LEV < 3
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		   Not available in this version	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	       MOVE "MNU\NOPMENU" TO PRG-NAME
	   ELSE
	       MOVE "DTP\DTPIMP"  TO PRG-NAME
	   END-IF.
	     PERFORM AA100.

       MA999.
	     EXIT.
      /
      *    ****   D E B T O R	P U R C H A S E	   H I S T O R Y
      *
       NA000	       SECTION 60.
       NA00.
	   IF LS0-LEV < 3
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		   Not available in this version	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	       MOVE "MNU\NOPMENU" TO PRG-NAME
	   ELSE
	       MOVE "DTP\DTPEXT"  TO PRG-NAME
	   END-IF.
003240	   IF LS0-DBLEV < 4
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO NA999
	   END-IF.
	     PERFORM AA100.

       NA999.
	     EXIT.
      /
      *    ****   D E B T O R	P U R C H A S E	   H I S T O R Y
      *
       OA000	       SECTION 61.
       OA00.
	   IF LS0-LEV < 3
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		   Not available in this version	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	       MOVE "MNU\NOPMENU" TO PRG-NAME
	   ELSE
	       MOVE "DTP\DTPPRC"  TO PRG-NAME
	   END-IF.
	     PERFORM AA100.

       OA999.
	     EXIT.
      /
      *    ****   D E B T O R	P A Y M E N T S    R E C E I V E D
      *
       PA000	       SECTION 62.
       PA00.
	   IF LS-DSKDRV = "L"
	       MOVE "Not available on Local drive"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO PA999
	   END-IF.
	   IF LS0-DBLEV < 2
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO PA999
	   END-IF.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 29		 TO SHADE-COL.
	     MOVE 20		 TO SHADE-WIDTH.
	     MOVE 8		 TO SHADE-LINES.
	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 0627
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 3
		     "¿" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 3.
	     DISPLAY "³ " AT 0727
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 3
		     "PAYMENTS  RECEIVED"
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 3
		     " ³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 3.
	     DISPLAY "³ " AT 0827
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 3
		     "1" WITH FOREGROUND-COLOR 7 HIGHLIGHT
			      BACKGROUND-COLOR 3
		     " Process Receipts ³"
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 3.
	     DISPLAY "³ " AT 0927
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 3
		     "2" WITH FOREGROUND-COLOR 7 HIGHLIGHT
			      BACKGROUND-COLOR 3
		     " Payment Reports  ³"
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 3.
	     DISPLAY "³ " AT 1027
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 3
		     "3" WITH FOREGROUND-COLOR 7 HIGHLIGHT
			      BACKGROUND-COLOR 3
		     " Allocate Credits ³"
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 3.
	     DISPLAY "³ " AT 1127
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 3
		     "4" WITH FOREGROUND-COLOR 7 HIGHLIGHT
			      BACKGROUND-COLOR 3
		     " Trial Balance    ³"
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 3.
	     DISPLAY "³ " AT 1227
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 3
		     "5" WITH FOREGROUND-COLOR 7 HIGHLIGHT
			      BACKGROUND-COLOR 3
		     " Age Analysis     ³"
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 3.
	     DISPLAY "³ " AT 1327
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 3
		     "0" WITH FOREGROUND-COLOR 7 HIGHLIGHT
			      BACKGROUND-COLOR 3
		     " Exit             ³"
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 3.
	     DISPLAY "À" AT 1427
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 3
		     "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 3.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.
	     MOVE 7		 TO TOP-ROW.
	     MOVE 12		 TO BOTTOM-ROW.
	     MOVE 6		 TO SCREEN-LIN.
	     MOVE 29		 TO SCREEN-COL.
	     MOVE 18		 TO STRING-LENGTH.
	     PERFORM SAVE-ATTR.

       PA05.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       PA10.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY	 PERFORM MOVE-BLOCK-UP
		 WHEN DOWN-KEY	 PERFORM MOVE-BLOCK-DOWN
		 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
				 IF MOUSE-ENTER = "Y"
				     MOVE "N"	 TO MOUSE-ENTER
				     GO TO PA15
				 END-IF
				 IF MOUSE-Y = 24
				     IF MOUSE-X = 5
					 MOVE 5  TO KEY-CODE-1
					 GO TO PA10
				     ELSE
				     IF MOUSE-X = 10
					 MOVE 6  TO KEY-CODE-1
					 GO TO PA10
				     ELSE
				     IF MOUSE-X > 36 AND < 44
					 MOVE 0  TO KEY-CODE-1
					 GO TO PA10
				     ELSE
				     IF MOUSE-X > 54 AND < 60
					 MOVE 0  TO KEY-CODE-1
					 MOVE 1  TO KEY-TYPE
					 GO TO PA10
				     END-IF
				 END-IF
		 WHEN ENTER-KEY  GO TO PA15
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO PA05
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE "0"	 TO WS-OPTION
		     GO TO PA18
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO PA05
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
	       IF WS-OPTION < 0 OR > 5
		   CALL X"E5"
		   GO TO PA05
	       END-IF
	       GO TO PA16
	   ELSE
	       CALL X"E5"
	       GO TO PA05
	   END-IF.

       PA15.
	     EVALUATE SCREEN-LIN
	       WHEN 7
		   MOVE "1"	 TO WS-OPTION
	       WHEN 8
		   MOVE "2"	 TO WS-OPTION
	       WHEN 9
		   MOVE "3"	 TO WS-OPTION
	       WHEN 10
		   MOVE "4"	 TO WS-OPTION
	       WHEN 11
		   MOVE "5"	 TO WS-OPTION
	       WHEN 12
		   MOVE "0"	 TO WS-OPTION
	       WHEN OTHER
		   CALL X"E5" CALL X"E5" CALL X"E5"
		   GO TO PA05
	     END-EVALUATE.
	   GO TO PA18.

       PA16.
	     PERFORM CLEAR-BLOCK.
	     EVALUATE WS-OPTION
	       WHEN "0"
		   MOVE 12	 TO SCREEN-LIN
	       WHEN "1"
		   MOVE 7	 TO SCREEN-LIN
	       WHEN "2"
		   MOVE 8	 TO SCREEN-LIN
	       WHEN "3"
		   MOVE 9	 TO SCREEN-LIN
	       WHEN "4"
		   MOVE 10	 TO SCREEN-LIN
	       WHEN "5"
		   MOVE 11	 TO SCREEN-LIN
	     END-EVALUATE.
	     PERFORM SAVE-ATTR.
	     PERFORM MARK-BLOCK.

       PA18.
	     EVALUATE WS-OPTION
	       WHEN "0"
		   GO TO PA999
	       WHEN "1"
		   MOVE "DTP\DTP012" TO PRG-NAME
	       WHEN "2"
		   MOVE "DTP\DTP019" TO PRG-NAME
	       WHEN "3"
		   MOVE "DTP\DTP013" TO PRG-NAME
	       WHEN "4"
		   MOVE "DTP\DTP020" TO PRG-NAME
	       WHEN "5"
		   MOVE "DTP\DTP026" TO PRG-NAME
	     END-EVALUATE.
	     PERFORM AA100.
	     DISPLAY MENU-INSTRUCT.
	     GO TO PA05.

       PA999.
	     EXIT.
      /
      *    ****   D E B T O R	E N Q U I R Y	( H I S T O R Y )
      *
       QA000	       SECTION 58.
       QA00.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		       Enquiries (History)		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE "DTP\DTPENH"	 TO PRG-NAME.
	     PERFORM AA100.

       QA999.
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
	       GO TO ZA00A
	   END-IF.
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
      *    ****    P A R A M E T E R   F I L E
      *    *************************************************************
       ZA00A.
	     MOVE "PARAM"	 TO AFID-KEY.

       ZA00-READ-APACFIDS.
000030	     READ APACFIDS WITH IGNORE LOCK
	       KEY IS AFID-KEY.
	   IF WS-STATUS = "00"
	       GO TO ZA00-READ-APACFIDS-EXIT
	   END-IF.
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
      *    ****    D E B T O R	 T R A N S A C T I O N	 F I L E    ****
      *    *************************************************************
	     MOVE "DBTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DBTRAN.
      *    *************************************************************
      *    **** 	  D E B T O R	M E M O	  F I L E	    ****
      *    *************************************************************
	     MOVE "DEBMEM"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBMEM.
      *    *************************************************************
      *    *** D E B T O R   A L T E R N A T E	 I N D E X   F I L E ***
      *    *************************************************************
	     MOVE "DEBALT"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBALT.
      *    *************************************************************
      *    ****  D E B T O R   A C C O U N T   ( A / R )   F I L E  ****
      *    *************************************************************
	     MOVE "DEBTOR"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBTOR.
      *    *************************************************************
      *    ****        D E B T O R   I N V O I C E   F I L E	    ****
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
      *    **** 	    C O N T R O L   F I L E		    ****
      *    *************************************************************
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
      *    *************************************************************
      *    **** 	       S T O C K   F I L E		    ****
      *    *************************************************************
	     MOVE "STOCKF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STOCKF.
025110	     MOVE 3		 TO WS-PARKEY.
	     READ PARAM.
	   IF PAR-STM = ZERO
	       MOVE 1		 TO PRG-NUMBER
	   ELSE
	       MOVE PAR-STM	 TO PRG-NUMBER
	   END-IF.
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
	       IF NOT (WS-STATUS = "41")
050210		   PERFORM OPEN-ERROR
	       END-IF
	   END-IF.
042210	     OPEN I-O DEBTOR.
042220     IF RUNTIME-ERROR
               IF FLE-LOCKED
042230             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
	       IF IDX-CORRUPT
                   MOVE 6        TO WS-F-ERROR
		   GO TO ZA50
	       END-IF
	   END-IF.
042240     IF WS-STATUS NOT = "00"
042250	       MOVE 6		 TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
050210		   PERFORM OPEN-ERROR
	       END-IF
	   END-IF.
042370	     OPEN I-O DBTRAN.
042380     IF RUNTIME-ERROR
               IF FLE-LOCKED
042390             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
	       IF IDX-CORRUPT
                   MOVE 5        TO WS-F-ERROR
		   GO TO ZA50
	       END-IF
	   END-IF.
042400     IF WS-STATUS NOT = "00"
042410	       MOVE 5		 TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
050210		   PERFORM OPEN-ERROR
	       END-IF
	   END-IF.
044300	     OPEN I-O DEBMEM.
           IF WS-STATUS = "05"
               CLOSE DEBMEM
               OPEN OUTPUT DEBMEM
               CLOSE DEBMEM
	       OPEN I-O DEBMEM
	   END-IF.
044310     IF RUNTIME-ERROR
               IF FLE-LOCKED
040420             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
	       IF IDX-CORRUPT
                   MOVE 27       TO WS-F-ERROR
		   GO TO ZA50
	       END-IF
	   END-IF.
	   IF WS-STATUS NOT = "00"
	       MOVE 27		 TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
		   PERFORM OPEN-ERROR
	       END-IF
	   END-IF.
054130	     OPEN I-O INVOICE.
054140     IF RUNTIME-ERROR
054150         IF FLE-LOCKED
054160             GO TO ZA200
054170         ELSE
054180         IF FLE-LIMIT
054190             GO TO ZA49
054200         ELSE
054210	       IF IDX-CORRUPT
054220             MOVE 12       TO WS-F-ERROR
054230		   GO TO ZA50
	       END-IF
	   END-IF.
054240     IF WS-STATUS NOT = "00"
054250         MOVE 12           TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
054260		   PERFORM OPEN-ERROR
	       END-IF
	   END-IF.
044300	     OPEN I-O LEDTRF.
           IF WS-STATUS = "05"
               CLOSE LEDTRF
               OPEN OUTPUT LEDTRF
               CLOSE LEDTRF
	       OPEN I-O LEDTRF
	   END-IF.
044310     IF RUNTIME-ERROR
               IF FLE-LOCKED
040420             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
	       IF IDX-CORRUPT
                   MOVE 40       TO WS-F-ERROR
		   GO TO ZA50
	       END-IF
	   END-IF.
050190     IF WS-STATUS NOT = "00"
050200         MOVE 40           TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
050210		   PERFORM OPEN-ERROR
	       END-IF
	   END-IF.
	   IF LS-DEM = 3
	       MOVE "DTP\DTP091" TO PRG-NAME
	       PERFORM AA100
	   END-IF.
039770	     GO TO ZA999.

       COPY ZA49.PRO.

039850 ZA50.
041850	   IF WS-F-ERROR = 5
041860         MOVE "DEBTOR TRAN"
                                 TO WS-FILE
           ELSE
055350	   IF WS-F-ERROR = 6
055360         MOVE "DEBTOR file"
055370                           TO WS-FILE
055380     ELSE
055350     IF WS-F-ERROR = 12
055360         MOVE "INVOICE file"
055370                           TO WS-FILE
           ELSE
041850     IF WS-F-ERROR = 27
041860         MOVE "DEBTOR MEMO"
				 TO WS-FILE
           ELSE
041850     IF WS-F-ERROR = 40
041860         MOVE "INTEGRATION"
				 TO WS-FILE
           ELSE
055350	   IF WS-F-ERROR = 47
055360	       MOVE "DEBALT file"
055370				 TO WS-FILE
	   END-IF.
039980       DISPLAY "Rebuild " AT 0812
039990		      WS-FILE WITH FOREGROUND-COLOR 6 HIGHLIGHT.
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
	       MOVE LS-USER	   TO WS-WRKST
	   END-IF.

040660 ZA200.

       COPY LOCKED.PRO.

040730 ZA205.
040740       EXIT PROGRAM.

040750 ZA999.
040760       EXIT.

040780 I-O-ERRORS      SECTION  91.
040930
       COPY ERRORS.PRO.

040950 DISPLAY-FILE-NAME.
041730	   IF WS-F-ERROR = 5
041740         MOVE W02-DBTRAN TO WS-FILE
041750         MOVE ZERO         TO WS-KEY
               MOVE TRA-KEY      TO WS-KEYX
041760     ELSE
041770     IF WS-F-ERROR = 6
041780         MOVE W02-DEBTOR TO WS-FILE
041790         MOVE ZERO         TO WS-KEY
               MOVE DEB-ACNO     TO WS-KEYX
041800     ELSE
041850	   IF WS-F-ERROR = 12
041860         MOVE W02-INVOIC TO WS-FILE
041870         MOVE ZERO      TO WS-KEY
               MOVE DOC-KEY      TO WS-KEYX
041840     ELSE
041850	   IF WS-F-ERROR = 27
041860         MOVE W02-DEBMEM   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
               MOVE DME-KEY      TO WS-KEYX
041840     ELSE
041850	   IF WS-F-ERROR = 40
041860         MOVE W02-LEDTRF   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE XFR-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 47
041860	       MOVE W02-DEBALT	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE DAL-KEY	 TO WS-KEYX
	   END-IF.

       COPY DISPERR.PRO.
