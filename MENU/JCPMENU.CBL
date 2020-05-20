      $set LINKCOUNT"768"
      ******************************************************************
      *                                                                *
      *  ********   ******   *******   **     **  ********  **	   **  *
      *     **	   **	 **  **    **  ***   ***  **	    ***    **  *
      *     **	   **	     **    **  ** * * **  **	    ** *   **  *
      *     **	   **	     *******   **  *  **  *****     **	*  **  *
      *     **	   **	     **        **     **  **	    **	 * **  *
      *  ** **	   **	 **  **        **     **  **	    **	  ***  *
      *   ***	    ******   **	       **     **  *******   **	   **  *
      *                                                                *
      *     ENGLISH						       *
      *                                                                *
      *     J O B   C O S T I N G   P R O G R A M   M E N U	       *
      *                                                                *
      *     VERSION 8.13.08 - August 2008			       *
      * 							       *
      ******************************************************************
      * 							       *
      *  September 2005 - Details of JOBS introduced. New sort key     *
      * 		  JOB-DETAILS = JOB-TYPE JOB-EXT-ITEM JOB-REF  *
      * 							       *
      *  May 2006	- New sort key (Lookup from Stock Item)        *
      * 		  JOB-CODE = JOB-EXT-ITEM JOB-REF	       *
      * 							       *
      *  Jan 2008	- New file (DEBALT) - Debtor alternate index   *
      * 		  included for lookups, using any word con-    *
      * 		  tained in the Debtor name.		       *
      * 							       *
      *  Aug 2008	- New file (STKALT) - Stock alternate index    *
      * 		  included for lookups, using any word con-    *
      * 		  tained in the Stock description.	       *
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
000020 PROGRAM-ID.     JCPMENU.
000030 AUTHOR.         J.W.LEMMON (APAC).
000040 DATE-WRITTEN.   JANUARY 1993.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1993 - 2008
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

       COPY CARDEX.SL.

000130 COPY CONTROL.SL.

       COPY CREDIT.SL.

000150 COPY DBTRAN.SL.

000160 COPY DEBALT.SL.

000170 COPY DEBTOR.ISL.

000140 COPY DEBMEM.SL.

000190 COPY DEPART.SL.

000270 COPY GARTEE.SL.

000270 COPY JOBAUD.SL.

000270 COPY JOBCST.SL.

000270 COPY JOBHDR.SL.

000270 COPY JOBREQ.SL.

000270 COPY JOBRMK.SL.

000270 COPY JOBWAR.SL.

       COPY LEDTRF.SL.

000210 COPY PARAM.SL.

000290 COPY SPARTS.SL.

       COPY STKALT.SL.

000140 COPY STKMEM.SL.

000310 COPY STOCK.SL.

       COPY TXTRAN.SL.

       COPY WSTOCK.SL.

      /
000330 DATA DIVISION.
000340 FILE SECTION.

       COPY APACFIDS.FDE.

000160 COPY CARDEX.FDE.

000360 COPY CONTROL.FDE.

       COPY CREDIT.FDE.

000380 COPY DBTRAN.FDE.

000330 COPY DEBALT.FDE.

000400 COPY DEBTOR.FDE.

000140 COPY DEBMEM.FDE.

000420 COPY DEPART.FDE.

000500 COPY GARTEE.FDE.

000500 COPY JOBAUD.FDE.

000500 COPY JOBCST.FDE.

000500 COPY JOBHDR.FDE.

000500 COPY JOBREQ.FDE.

000500 COPY JOBRMK.FDE.

000270 COPY JOBWAR.FDE.

       COPY LEDTRF.FDE.

000440 COPY PARAM.FDE.

000520 COPY SPARTS.FDE.

       COPY STKALT.FDE.

000140 COPY STKMEM.FDE.

000540 COPY STOCK.FDE.

       COPY TXTRAN.FDE.

       COPY WSTOCK.FDE.

      /
000560 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
000610 77  WS-S1	   PIC	9(04)	 COMP-5.
000620 77  WS-S2           PIC  9(04)    COMP-5.
000630 77  WS-S3           PIC  9(04)    COMP-5.
000640 77  WS-S4           PIC  9(04)    COMP-5.
000770 77  WS-DEPKEY       PIC  9(04)    COMP-5.
000820 77  WS-NETKEY       PIC  9(04)    COMP-5.
000750 77  WS-PARKEY       PIC  9(04)    COMP-5.
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

000160 COPY CARDEX.ID.

002060 COPY CONTROL.ID.

       COPY CREDIT.ID.

002080 COPY DBTRAN.ID.

000330 COPY DEBALT.ID.

002100 COPY DEBTOR.ID.

000140 COPY DEBMEM.ID.

002120 COPY DEPART.ID.

002180 COPY GARTEE.ID.

000500 COPY JOBAUD.ID.

002180 COPY JOBCST.ID.

002180 COPY JOBHDR.ID.

002180 COPY JOBREQ.ID.

002180 COPY JOBRMK.ID.

000270 COPY JOBWAR.ID.

       COPY LEDTRF.ID.

       COPY PARAM.ID.

002200 COPY SPARTS.ID.

       COPY STKALT.ID.

000140 COPY STKMEM.ID.

002220 COPY STOCK.ID.

       COPY TXTRAN.ID.

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
001450	   03  LINE  2 COLUMN 26 BACKGROUND-COLOR 1
				 FOREGROUND-COLOR 7 HIGHLIGHT
001460				 VALUE "JOB COSTING / WORK IN PROGRESS".

       01  S02.
	   03  LINE  4 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
	   03	       COLUMN 27 BACKGROUND-COLOR 2 FOREGROUND-COLOR 0
				 VALUE "¿".
	   03  LINE  5 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "JOBS PROGRAM MENU".
	   03	       COLUMN 26 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " ³".
	   03  LINE  6 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "1".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Job Costing	 ³".
001470	   03  LINE  7 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "2".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Process Job	 ³".
001570	   03  LINE  8 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "3".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Job Types	 ³".
001500	   03  LINE  9 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "4".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Enquiries	 ³".
	   03  LINE 10 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "5".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Reports	 ³".
	   03  LINE 11 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "6".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Purge History   ³".
	   03  LINE 12 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "7".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Job Card Remarks³".
	   03  LINE 13 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "0".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Exit	         ³".
	   03  LINE 14 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "À".
	   03	       COLUMN  8 BACKGROUND-COLOR 2 FOREGROUND-COLOR 0
				 VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S03.
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
				 VALUE "QUOTATIONS".
	   03	       COLUMN 41 BACKGROUND-COLOR 3
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
				 VALUE " New Quote      ³".
001470	   03  LINE  9 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "2".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Amend Quote    ³".
001570	   03  LINE 10 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "3".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Delete Quote   ³".
001500	   03  LINE 11 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "4".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Convert to Job ³".
	   03  LINE 12 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "5".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Quote Remarks  ³".
	   03  LINE 13 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "0".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Exit	        ³".
	   03  LINE 14 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "À".
	   03	       COLUMN 27 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
				 VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S04.
	   03  LINE  6 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
	   03	       COLUMN 45 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
				 VALUE "¿".
	   03  LINE  7 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³   ".
	   03	       COLUMN 30 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "JOB COSTING".
	   03	       COLUMN 41 BACKGROUND-COLOR 3
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
				 VALUE " New Job        ³".
001470	   03  LINE  9 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "2".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Amend Job      ³".
001570	   03  LINE 10 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "3".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Requisitions   ³".
001500	   03  LINE 11 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "4".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Labour/Remarks ³".
	   03  LINE 12 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "5".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Cancel Job     ³".
	   03  LINE 13 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "6".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Amend Status   ³".
	   03  LINE 14 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "7".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Re-Print JobReq³".
	   03  LINE 15 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "0".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Exit	        ³".
	   03  LINE 16 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "À".
	   03	       COLUMN 27 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
				 VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".
       01  S05.
	   03  LINE  8 COLUMN 44 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
	   03	       COLUMN 63 BACKGROUND-COLOR 7 FOREGROUND-COLOR 0
				 VALUE "¿".
	   03  LINE  9 COLUMN 44 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³   ".
	   03	       COLUMN 48 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "REQUISITIONS".
	   03	       COLUMN 60 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE "   ³".
	   03  LINE 10 COLUMN 44 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 46 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "1".
	   03	       COLUMN 47 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Order Spares   ³".
	   03  LINE 11 COLUMN 44 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 46 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "2".
	   03	       COLUMN 47 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Delete         ³".
	   03  LINE 12 COLUMN 44 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 46 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "3".
	   03	       COLUMN 47 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Receive Spares ³".
	   03  LINE 13 COLUMN 44 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 46 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "4".
	   03	       COLUMN 47 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Allocate to job³".
	   03  LINE 14 COLUMN 44 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 46 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "5".
	   03	       COLUMN 47 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Print Req Slip ³".
	   03  LINE 15 COLUMN 44 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 46 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "0".
	   03	       COLUMN 47 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 VALUE " Exit           ³".
	   03  LINE 16 COLUMN 44 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "À".
	   03	       COLUMN 45 BACKGROUND-COLOR 7 FOREGROUND-COLOR 0
				 VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S06.
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
				 VALUE "JOB TYPES".
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
				 VALUE " Create/Amend   ³".
001470	   03  LINE  9 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "2".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Delete         ³".
001570	   03  LINE 10 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "3".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Print List     ³".
	   03  LINE 11 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "0".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Exit	        ³".
001500	   03  LINE 12 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "À".
	   03	       COLUMN 27 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
				 VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

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

       01  S08.
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
				 VALUE "JOB REPORTS".
	   03	       COLUMN 41 BACKGROUND-COLOR 3
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
				 VALUE " Not Started    ³".
001470	   03  LINE  9 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "2".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " In Progress    ³".
001570	   03  LINE 10 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "3".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Waiting        ³".
	   03  LINE 11 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "4".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Completed      ³".
      *	   03  LINE 12 COLUMN 26 BACKGROUND-COLOR 3
      *				 FOREGROUND-COLOR 3 HIGHLIGHT
      *				 VALUE "³ ".
      *	   03	       COLUMN 28 BACKGROUND-COLOR 3
      *				 FOREGROUND-COLOR 7
      *				 HIGHLIGHT
      *				 VALUE "5".
      *	   03	       COLUMN 29 BACKGROUND-COLOR 3
      *				 FOREGROUND-COLOR 0
      *				 VALUE " Quotations     ³".
	   03  LINE 12 COLUMN 26 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "0".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Exit           ³".
	   03  LINE 13 COLUMN 26 BACKGROUND-COLOR 3
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
003240	   IF LS0-JCP NOT = 2
	       MOVE "Not Installed"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	   ELSE
003240	   IF LS0-JOBS = "N"
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
013040	     CLOSE CARDEX
		   CREDIT
		   DBTRAN
013070		   DEBALT
		   DEBTOR
                   DEBMEM
		   GARTEE
013110		   JOBAUD
013110             JOBCST 
013110		   JOBHDR
013110		   JOBREQ
013110		   JOBRMK
013110		   JOBWAR
		   LEDTRF
013120             SPARTS
		   STKALT
                   STKMEM
013130		   STOCK
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
	     PERFORM ERROR-LENGTH THRU ERROR-EXIT.
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

       COPY ERROR.SCR.

       COPY LOCKED.RC2.

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

011410 COPY CLEAR.CRT.

011750 COPY PARAM.RD.

       BA000	       SECTION.
       BA00.
	     MOVE 5		 TO SHADE-ROW.
	     MOVE 9		 TO SHADE-COL.
	     MOVE 19		 TO SHADE-WIDTH.
	     MOVE 10		 TO SHADE-LINES.
	     DISPLAY S02.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.
       BA02.
	     MOVE 5		 TO TOP-ROW.
	     MOVE 12		 TO BOTTOM-ROW.
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
	       IF WS-OPTION < 0 OR > 7
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
	   IF SCREEN-LIN = 11
	       MOVE "0"		 TO WS-OPTION.
	   GO TO BA18.

       BA16.
	     PERFORM CLEAR-BLOCK.
	   IF WS-OPTION = "0"
	       MOVE 12		 TO SCREEN-LIN
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
	       MOVE 11		 TO SCREEN-LIN.
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
	       PERFORM DA000
	   ELSE
	   IF WS-OPTION = "2"
	       PERFORM CA000
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
	       PERFORM IA000.
	     PERFORM RESTORE-SCREEN.
	     MOVE BLOCK-DETAIL1  TO BLOCK-DETAIL.
	     MOVE SHADOW-DETAIL1 TO SHADOW-DETAIL.
	     MOVE CRT-DETAIL1	 TO CRT-DETAIL.
	     GO TO BA05.
       BA999.
	     EXIT.
      /
      *    ****    P R O C E S S   A   J O B
      *
       CA000	       SECTION 50.
       CA00.
	   IF LS-DSKDRV = "L"
	       MOVE "Not available on Local drive"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO CA999.
	     MOVE "JCP\JOB001"	 TO PRG-NAME.
	     PERFORM AA100.
	     DISPLAY MENU-INSTRUCT.
       CA999.
	     EXIT.
      /
      *    ****    J O B   C O S T I N G
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
	     MOVE 18		 TO SHADE-WIDTH.
	     MOVE 10		 TO SHADE-LINES.
	     DISPLAY S04.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.
	     MOVE 7		 TO TOP-ROW.
	     MOVE 14		 TO BOTTOM-ROW.
	     MOVE 6		 TO SCREEN-LIN.
	     MOVE 28		 TO SCREEN-COL.
	     MOVE 16		 TO STRING-LENGTH.
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
	   IF WS-OPTION = "1"
	       MOVE "JCP\JCP001" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "2"
	       MOVE "JCP\JCP002" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "3"
	       PERFORM SAVE-SCREEN-2
	       MOVE BLOCK-DETAIL TO BLOCK-DETAIL2
	       MOVE SHADOW-DETAIL
				 TO SHADOW-DETAIL2
	       MOVE CRT-DETAIL	 TO CRT-DETAIL2
	       PERFORM DB000
	       PERFORM RESTORE-SCREEN-2
	       MOVE BLOCK-DETAIL2
				 TO BLOCK-DETAIL
	       MOVE SHADOW-DETAIL2
				 TO SHADOW-DETAIL
	       MOVE CRT-DETAIL2	 TO CRT-DETAIL
	       GO TO DA05
	   ELSE
	   IF WS-OPTION = "4"
	       MOVE "JCP\JCP003" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "5"
	       MOVE "JCP\JCP004" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "6"
	       MOVE "JCP\JCP005" TO PRG-NAME
	   ELSE
	       MOVE "JCP\JCP006" TO PRG-NAME.
	     PERFORM AA100.
	     DISPLAY MENU-INSTRUCT.
	     GO TO DA05.
       DA999.
	     EXIT.
      *
      *    ****    S P A R E S	 R E Q U I S I T I O N S
      *
       DB000	       SECTION 51.
       DB00.
	     MOVE 9		 TO SHADE-ROW.
	     MOVE 46		 TO SHADE-COL.
	     MOVE 18		 TO SHADE-WIDTH.
	     MOVE 8		 TO SHADE-LINES.
	     DISPLAY S05.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.
	     MOVE 9		 TO TOP-ROW.
	     MOVE 14		 TO BOTTOM-ROW.
	     MOVE 8		 TO SCREEN-LIN.
	     MOVE 46		 TO SCREEN-COL.
	     MOVE 16		 TO STRING-LENGTH.
	     PERFORM SAVE-ATTR.

       DB05.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       DB10.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY	 PERFORM MOVE-BLOCK-UP
		 WHEN DOWN-KEY	 PERFORM MOVE-BLOCK-DOWN
		 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
				 IF MOUSE-ENTER = "Y"
				     MOVE "N"	 TO MOUSE-ENTER
				     GO TO DB15
				 END-IF
				 IF MOUSE-Y = 24
				     IF MOUSE-X = 5
					 MOVE 5  TO KEY-CODE-1
					 GO TO DB10
				     ELSE
				     IF MOUSE-X = 10
					 MOVE 6  TO KEY-CODE-1
					 GO TO DB10
				     ELSE
				     IF MOUSE-X > 36 AND < 44
					 MOVE 0  TO KEY-CODE-1
					 GO TO DB10
				     ELSE
				     IF MOUSE-X > 54 AND < 60
					 MOVE 0  TO KEY-CODE-1
					 MOVE 1  TO KEY-TYPE
					 GO TO DB10
				     END-IF
				 END-IF
		 WHEN ENTER-KEY  GO TO DB15
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO DB05
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE "0"	 TO WS-OPTION
		     GO TO DB18
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO DB05
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	       IF WS-OPTION < 0 OR > 5
		   CALL X"E5"
		   GO TO DB05
	       END-IF
	       GO TO DB16
	   ELSE
	       CALL X"E5"
	       GO TO DB05.

       DB15.
	   IF SCREEN-LIN < 9
	       CALL X"E5"
	       GO TO DB05
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
	   GO TO DB18.

       DB16.
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

       DB18.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF WS-OPTION = "0"
	       GO TO DB999.
	   IF WS-OPTION < 1 OR > 5
	       CALL X"E5"
	       GO TO DB05.
	   IF WS-OPTION = "1"
	       MOVE "JCP\JBR001" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "2"
	       MOVE "JCP\JBR002" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "3"
	       MOVE "JCP\JBR003" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "4"
	       MOVE "JCP\JBR004" TO PRG-NAME
	   ELSE
	       MOVE "JCP\JBR005" TO PRG-NAME.
	     PERFORM AA100.
	     DISPLAY MENU-INSTRUCT.
	     GO TO DB05.

       DB999.
	     EXIT.
      /
      *    ****    J O B   T Y P E S
      *
       EA000	       SECTION 52.
       EA00.
	   IF LS-DSKDRV = "L"
	       MOVE "Not available on Local drive"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO EA999.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 28		 TO SHADE-COL.
	     MOVE 18		 TO SHADE-WIDTH.
	     MOVE 6		 TO SHADE-LINES.
	     DISPLAY S06.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.
	     MOVE 7		 TO TOP-ROW.
	     MOVE 11		 TO BOTTOM-ROW.
	     MOVE 6		 TO SCREEN-LIN.
	     MOVE 28		 TO SCREEN-COL.
	     MOVE 16		 TO STRING-LENGTH.
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
	       IF WS-OPTION < 0 OR > 3
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
	       MOVE "0"		 TO WS-OPTION.
	   GO TO EA18.

       EA16.
	     PERFORM CLEAR-BLOCK.
	   IF WS-OPTION = 0
	       MOVE 10		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = 1
	       MOVE 7		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = 2
	       MOVE 8		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = 3
	       MOVE 9		 TO SCREEN-LIN.
	     PERFORM SAVE-ATTR.
	     PERFORM MARK-BLOCK.

       EA18.
	   IF WS-OPTION = "0"
	       GO TO EA999.
	   IF WS-OPTION = "1"
	       MOVE "JCP\JBT001" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "2"
	       MOVE "JCP\JBT002" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "3"
	       MOVE "JCP\JBT003" TO PRG-NAME.
	     PERFORM AA100.
	     DISPLAY MENU-INSTRUCT.
	     GO TO EA05.

       EA999.
	     EXIT.
      /
      *    ****    E N Q U I R I E S
      *
       FA000	       SECTION 53.
       FA00.
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
	       IF WS-OPTION < 0 OR > 2
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
	       MOVE "0"		 TO WS-OPTION.
	   GO TO FA18.

       FA16.
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

       FA18.
	   IF WS-OPTION = "0"
	       GO TO FA999.
	   IF WS-OPTION = "1"
	       MOVE "DTP\DTPENQ" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "2"
	       MOVE "STP\STPENQ" TO PRG-NAME.
	     PERFORM AA100.
	     DISPLAY MENU-INSTRUCT.
	     GO TO FA05.

       FA999.
	     EXIT.
      /
      *    ****    J O B   R E P O R T S
      *
       GA000	       SECTION 54.
       GA00.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 28		 TO SHADE-COL.
	     MOVE 18		 TO SHADE-WIDTH.
	     MOVE 7		 TO SHADE-LINES.
	     DISPLAY S08.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.
	     MOVE 7		 TO TOP-ROW.
	     MOVE 11		 TO BOTTOM-ROW.
	     MOVE 6		 TO SCREEN-LIN.
	     MOVE 28		 TO SCREEN-COL.
	     MOVE 16		 TO STRING-LENGTH.
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
	       IF WS-OPTION < 0 OR > 4
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
	       MOVE "0"		 TO WS-OPTION.
	   GO TO GA18.

       GA16.
	     PERFORM CLEAR-BLOCK.
	   IF WS-OPTION = 0
	       MOVE 11		 TO SCREEN-LIN
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

       GA18.
	   IF WS-OPTION = "0"
	       GO TO GA999.
	   IF WS-OPTION = "1"
	       MOVE "JCP\JBL001" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "2"
	       MOVE "JCP\JBL002" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "3"
	       MOVE "JCP\JBL003" TO PRG-NAME
	   ELSE
	   IF WS-OPTION = "4"
	       MOVE "JCP\JBL004" TO PRG-NAME.
	     PERFORM AA100.
	     DISPLAY MENU-INSTRUCT.
	     GO TO GA05.

       GA999.
	     EXIT.
      /
      *    ****    J O B   D E L E T I O N S
      *
       HA000	       SECTION 55.
       HA00.
	   IF LS-DSKDRV = "L"
	       MOVE "Not available on Local drive"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO HA999.
	     MOVE "JCP\JCP007"	 TO PRG-NAME.
	     PERFORM AA100.
	     DISPLAY MENU-INSTRUCT.

       HA999.
	     EXIT.
      /
      *    ****    J O B   R E M A R K S
      *
       IA000	       SECTION 55.
       IA00.
	   IF LS-DSKDRV = "L"
	       MOVE "Not available on Local drive"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO IA999.
	     MOVE "JCP\JCPRMK"	 TO PRG-NAME.
	     PERFORM AA100.
	     DISPLAY MENU-INSTRUCT.

       IA999.
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
      *    ****    D O	 N O T	 D I S P L A Y	 E R R O R
      * 	   M E S S A G E S
      *
      *	     MOVE 44		 TO USER-NUMBER.
      *	     MOVE 1		 TO USER-SETTING.
      *	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
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
      *    PRE-DISPLAY ACCEPT FIELDS BEFORE DATA-ENTRY
      *
      *      MOVE 76		 TO USER-NUMBER.
      *      MOVE 3		 TO USER-SETTING.
      *      MOVE 1		 TO USER-KEYS.
      *      CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *
      *    ****    A C T I V A T E   M O U S E
      *
	     MOVE 64		 TO MOUSE-FUNC.
	     MOVE 1		 TO MOUSE-PARAM.
	     CALL X"AF" USING MOUSE-FUNC
			      MOUSE-PARAM.
	   IF MOUSE-FUNC NOT = 255
	       MOVE "Y"		 TO MOUSE-ATTACHED
      *        MOVE 66		 TO MOUSE-FUNC
      *        MOVE 0		 TO MOUSE-PARAM
      *        CALL X"AF" USING MOUSE-FUNC
      *				MOUSE-PARAM
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
      *      CALL CBL_INIT_MOUSE USING MOUSE-HANDLE
      *				       MOUSE-BUTTONS
      *				 RETURNING MOUSE-STATUS.
      *    IF MOUSE-STATUS = ZERO
      *        MOVE "Y"		 TO MOUSE-ATTACHED.
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
      *    **** 	   D E P A R T M E N T	 F I L E	    ****
      *    *************************************************************
	     MOVE "DEPART"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEPART.
      *    *************************************************************
      *    **** 	    G U A R A N T E E	F I L E		    ****
      *    *************************************************************
	     MOVE "GARTEE"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-GARTEE.
      *    *************************************************************
      *    ****   J O B   C O S T I N G   -   A U D I T   F I L E   ****
      *    *************************************************************
	     MOVE "JOBAUD"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-JOBAUD.
      *    *************************************************************
      *    **** J O B	C O S T I N G	-   D E T A I L S   F I L E ****
      *    *************************************************************
	     MOVE "JOBCST"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-JOBCST.
      *    *************************************************************
      *    ****	 J O B	 C O S T I N G	 -   H E A D E R   F I L E  ****
      *    *************************************************************
	     MOVE "JOBHDR"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-JOBHDR.
      *    *************************************************************
      *     J O B   C O S T I N G   -	R E Q U I S I T I O N	F I L E
      *    *************************************************************
	     MOVE "JOBREQ"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-JOBREQ.
      *    *************************************************************
      *    ***	J O B	C O S T I N G	-   R E M A R K S   F I L E  ***
      *    *************************************************************
	     MOVE "JOBRMK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-JOBRMK.
	     MOVE "JOBWAR"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-JOBWAR.
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
      *    ****       V A L U E	  A D D E D   T A X   F I L E	    ****
      *    *************************************************************
	     MOVE "TXTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-TXTRAN.
      *    *************************************************************
      *    W A R E H O U S E  -  S T O C K  ( I V E N T O R Y )  F I L E
      *    *************************************************************
	     MOVE "WSTOCK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-WSTOCK.
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
042240	     OPEN I-O CREDIT.
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
038570	     OPEN I-O DBTRAN.
038580     IF RUNTIME-ERROR
038590         IF FLE-LOCKED
038600             GO TO ZA200
038610         ELSE
038620	       IF FLE-LIMIT
038630		   GO TO ZA49
               ELSE
	       IF IDX-CORRUPT
		   MOVE 5	 TO WS-F-ERROR
		   GO TO ZA50.
038640     IF WS-STATUS NOT = "00"
038650         MOVE 5            TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
038660		   PERFORM OPEN-ERROR.
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
038670	     OPEN I-O DEBTOR.
038680     IF RUNTIME-ERROR
038690         IF FLE-LOCKED
038700             GO TO ZA200
038710         ELSE
038720         IF FLE-LIMIT
038730             GO TO ZA49
038740         ELSE
038750	       IF IDX-CORRUPT
038760		   MOVE 6	 TO WS-F-ERROR
038770		   GO TO ZA50.
038780     IF WS-STATUS NOT = "00"
038790         MOVE 6            TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
038800		   PERFORM OPEN-ERROR.
044300       OPEN I-O DEBMEM.
044310     IF RUNTIME-ERROR
               IF FLE-LOCKED
040420             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
	       IF IDX-CORRUPT
		   MOVE 27	 TO WS-F-ERROR
                   GO TO ZA50.
038910	   IF WS-STATUS NOT = "00"
038920	       MOVE 27		 TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
038930		   PERFORM OPEN-ERROR.
038810	     OPEN I-O GARTEE.
038820     IF RUNTIME-ERROR
038830         IF FLE-LOCKED
038840             GO TO ZA200
038850         ELSE
038860         IF FLE-LIMIT
038870		   GO TO ZA49
039040         ELSE
039050	       IF IDX-CORRUPT
039060		   MOVE 9	 TO WS-F-ERROR
039070		   GO TO ZA50.
038880     IF WS-STATUS NOT = "00"
038890	       MOVE 9		 TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
038900		   PERFORM OPEN-ERROR.
038970	     OPEN I-O JOBAUD.
038980     IF RUNTIME-ERROR
038990         IF FLE-LOCKED
039000             GO TO ZA200
039010         ELSE
039020         IF FLE-LIMIT
039030             GO TO ZA49
039040         ELSE
039050	       IF IDX-CORRUPT
039060		   MOVE 24	 TO WS-F-ERROR
039070             GO TO ZA50.
039080     IF WS-STATUS NOT = "00"
039090	       MOVE 24		 TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
039100		   PERFORM OPEN-ERROR.
038970	     OPEN I-O JOBCST.
038980     IF RUNTIME-ERROR
038990         IF FLE-LOCKED
039000             GO TO ZA200
039010         ELSE
039020         IF FLE-LIMIT
039030             GO TO ZA49
039040         ELSE
039050	       IF IDX-CORRUPT
039060		   MOVE 13	 TO WS-F-ERROR
039070             GO TO ZA50.
039080     IF WS-STATUS NOT = "00"
039090         MOVE 13           TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
039100		   PERFORM OPEN-ERROR.
038970	     OPEN I-O JOBHDR.
038980     IF RUNTIME-ERROR
038990         IF FLE-LOCKED
039000             GO TO ZA200
039010         ELSE
039020         IF FLE-LIMIT
039030             GO TO ZA49
039040         ELSE
039050	       IF IDX-CORRUPT
039060		   MOVE 14	 TO WS-F-ERROR
039070             GO TO ZA50.
039080     IF WS-STATUS NOT = "00"
039090	       MOVE 14		 TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
039100		   PERFORM OPEN-ERROR.
038970	     OPEN I-O JOBWAR.
038980     IF RUNTIME-ERROR
038990         IF FLE-LOCKED
039000             GO TO ZA200
039010         ELSE
039020         IF FLE-LIMIT
039030             GO TO ZA49
039040         ELSE
039050	       IF IDX-CORRUPT
039060		   MOVE 28	 TO WS-F-ERROR
039070             GO TO ZA50.
039080     IF WS-STATUS NOT = "00"
039090	       MOVE 28		 TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
039100		   PERFORM OPEN-ERROR.
039110	     OPEN I-O SPARTS.
039120     IF RUNTIME-ERROR
039130         IF FLE-LOCKED
039140             GO TO ZA200
039150         ELSE
039160         IF FLE-LIMIT
039170             GO TO ZA49
039180         ELSE
039190	       IF IDX-CORRUPT
039200		   MOVE 21	 TO WS-F-ERROR
039210             GO TO ZA50.
039220     IF WS-STATUS NOT = "00"
039230         MOVE 21           TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
039240		   PERFORM OPEN-ERROR.
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
039250       OPEN I-O STOCK.
039260     IF RUNTIME-ERROR
039270         IF FLE-LOCKED
039280             GO TO ZA200
039290         ELSE
039300         IF FLE-LIMIT
039310             GO TO ZA49
039320         ELSE
039330	       IF IDX-CORRUPT
039340             MOVE 22       TO WS-F-ERROR
039350             GO TO ZA50.
039360     IF WS-STATUS NOT = "00"
039370         MOVE 22           TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
039380		   PERFORM OPEN-ERROR.
038670	     OPEN I-O LEDTRF.
038680     IF RUNTIME-ERROR
038690         IF FLE-LOCKED
038700             GO TO ZA200
038710         ELSE
038720         IF FLE-LIMIT
038730             GO TO ZA49
038740         ELSE
038750	       IF IDX-CORRUPT
038760		   MOVE 40	 TO WS-F-ERROR
038770             GO TO ZA50.
038780     IF WS-STATUS NOT = "00"
038790	       MOVE 40		 TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
038800		   PERFORM OPEN-ERROR.
038670	     OPEN I-O JOBRMK.
038680     IF RUNTIME-ERROR
038690         IF FLE-LOCKED
038700             GO TO ZA200
038710         ELSE
038720         IF FLE-LIMIT
038730             GO TO ZA49
038740         ELSE
038750	       IF IDX-CORRUPT
038760             MOVE 41       TO WS-F-ERROR
038770             GO TO ZA50.
038780     IF WS-STATUS NOT = "00"
038790         MOVE 41           TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
038800		   PERFORM OPEN-ERROR.
038670	     OPEN I-O JOBREQ.
           IF WS-STATUS = "05"
               CLOSE JOBREQ
               OPEN OUTPUT JOBREQ
               CLOSE JOBREQ
               OPEN I-O JOBREQ.
038680	   IF RUNTIME-ERROR
038690         IF FLE-LOCKED
038700             GO TO ZA200
038710         ELSE
038720         IF FLE-LIMIT
038730             GO TO ZA49
038740         ELSE
038750	       IF IDX-CORRUPT
038760		   MOVE 42	 TO WS-F-ERROR
038770             GO TO ZA50.
038780     IF WS-STATUS NOT = "00"
038790	       MOVE 42		 TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
038800		   PERFORM OPEN-ERROR.
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
039860	   IF WS-F-ERROR = 6
039870         MOVE "DEBTOR file"
039880                           TO WS-FILE
039890     ELSE
039860	   IF WS-F-ERROR = 9
039870	       MOVE "GUARANTEE file"
039880                           TO WS-FILE
039890     ELSE
039900	   IF WS-F-ERROR = 13 OR 14
039910         MOVE "JOB file"   TO WS-FILE
039920     ELSE
039930     IF WS-F-ERROR = 21
039940         MOVE "PRODUCTION" TO WS-FILE
039950     ELSE
039960     IF WS-F-ERROR = 22
039970         MOVE "STOCK file" TO WS-FILE
039950	   ELSE
039960	   IF WS-F-ERROR = 24
039970	       MOVE "JOB AUDIT"	 TO WS-FILE
	   ELSE
           IF WS-F-ERROR = 25
               MOVE "STOCK MEMO" TO WS-FILE
	   ELSE
041850     IF WS-F-ERROR = 27
041860         MOVE "DEBTOR MEMO"
                                 TO WS-FILE
	   ELSE
041850	   IF WS-F-ERROR = 28
041860	       MOVE "REV.WARRANTY"
                                 TO WS-FILE
041150	   ELSE
039860	   IF WS-F-ERROR = 32
039870	       MOVE "TAX file"	 TO WS-FILE
041150	   ELSE
039860	   IF WS-F-ERROR = 40
039870	       MOVE "INTEGRATION"
				 TO WS-FILE
041150	   ELSE
039860     IF WS-F-ERROR = 41
039870         MOVE "JOB TYPE file"
				 TO WS-FILE
041150     ELSE
039860	   IF WS-F-ERROR = 42
039870	       MOVE "REQUISITIONS"
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
040960     IF WS-F-ERROR = 2
040970         MOVE W02-NETWORK TO WS-FILE
040980         MOVE WS-NETKEY TO WS-KEY
040990     ELSE
041000     IF WS-F-ERROR = 5
041010         MOVE W02-DBTRAN   TO WS-FILE
041020         MOVE ZERO         TO WS-KEY
               MOVE TRA-KEY      TO WS-KEYX
041030     ELSE
041040     IF WS-F-ERROR = 6
041050         MOVE W02-DEBTOR   TO WS-FILE
041060         MOVE ZERO         TO WS-KEY
               MOVE DEB-ACNO     TO WS-KEYX
041070     ELSE
041080     IF WS-F-ERROR = 7
041090         MOVE W02-DEPART   TO WS-FILE
041100         MOVE WS-DEPKEY    TO WS-KEY
	   ELSE
           IF WS-F-ERROR = 9
               MOVE W02-GARTEE   TO WS-FILE
               MOVE ZERO         TO WS-KEY
	       MOVE GUA-KEY	 TO WS-KEYX
041230	   ELSE
041240     IF WS-F-ERROR = 13
041250         MOVE W02-JOBCST   TO WS-FILE
041260         MOVE ZERO         TO WS-KEY
               MOVE JOB-KEY      TO WS-KEYX
041110	   ELSE
041240	   IF WS-F-ERROR = 14
041250	       MOVE W02-JOBHDR	 TO WS-FILE
041260         MOVE ZERO         TO WS-KEY
	       MOVE JHD-KEY2	 TO WS-KEYX
041110	   ELSE
041120     IF WS-F-ERROR = 15
041130         MOVE WS-PARID     TO WS-FILE
041140         MOVE WS-PARKEY    TO WS-KEY
041270	   ELSE
041280     IF WS-F-ERROR = 21
041290         MOVE W02-SPARTS   TO WS-FILE
041300         MOVE ZERO         TO WS-KEY
               MOVE PRT-ITEM     TO WS-KEYX
041310     ELSE
041320     IF WS-F-ERROR = 22
041330         MOVE W02-STOCKF   TO WS-FILE
041340         MOVE ZERO         TO WS-KEY
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
               MOVE DME-KEY      TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 28
041860	       MOVE W02-JOBWAR	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE JBW-KEY	 TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 32
041860         MOVE W02-TXTRAN   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
               MOVE TAX-KEY      TO WS-KEYX
041840     ELSE
041850     IF WS-F-ERROR = 40
041860         MOVE W02-LEDTRF   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
               MOVE XFR-KEY      TO WS-KEYX
041150	   ELSE
041240     IF WS-F-ERROR =  41
041250	       MOVE W02-JOBRMK	 TO WS-FILE
041260         MOVE ZERO         TO WS-KEY
	       MOVE JRM-KEY	 TO WS-KEYX
041150     ELSE
041240	   IF WS-F-ERROR =  42
041250	       MOVE W02-JOBREQ	 TO WS-FILE
041260         MOVE ZERO         TO WS-KEY
	       MOVE REQ-KEY	 TO WS-KEYX
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
	       MOVE STKA-KEY	 TO WS-KEYX.
041350     IF WS-STATUS = "10"
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
