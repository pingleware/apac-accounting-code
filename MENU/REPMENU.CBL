      $set LINKCOUNT"384"
      ******************************************************************
      *                                                                *
      *  *******   ********  *******   **     **  ********  **	   **  *
      *  **    **  **	     **	   **  ***   ***  **	    ***	   **  *
      *  **    **  **	     **	   **  ** * * **  **	    ** *   **  *
      *  *******   *****     *******   **  *  **  *****	    **	*  **  *
      *  **   **   **	     **	       **     **  **	    **	 * **  *
      *  **    **  **	     **	       **     **  **	    **	  ***  *
      *  **    **  ********  **        **     **  *******   **	   **  *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     P E R I O D   E N D   M O D U L E	M E N U 	       *
      *                                                                *
      *     VERSION 8.15.02 - December 2011			       *
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
000020 PROGRAM-ID.     REPMENU.
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

000130 COPY CONTROL.SL.

000190 COPY DEPART.SL.

000210 COPY PARAM.SL.

       COPY TXTRAN.SL.

      /
000330 DATA DIVISION.
000340 FILE SECTION.

       COPY APACFIDS.FDE.

000360 COPY CONTROL.FDE.

000420 COPY DEPART.FDE.

000440 COPY PARAM.FDE.

       COPY TXTRAN.FDE.

      /
000560 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
000610 77  WS-S1	   PIC	9(04)	 COMP-5.
000620 77  WS-S2           PIC  9(04)    COMP-5.
000630 77  WS-S3           PIC  9(04)    COMP-5.
000640 77  WS-S4           PIC  9(04)    COMP-5.
000770 77  WS-DEPKEY	   PIC	9(04)	 COMP-5.
000820 77  WS-NETKEY       PIC  9(04)    COMP-5.
000750 77  WS-PARKEY       PIC  9(04)    COMP-5.
       77  WS-TEMP-LIN	   PIC	9(02)	 COMP-5.
       77  WS-OPTION	   PIC	X(01).
       77  PRG-NAME	   PIC	X(12) VALUE SPACES.
       77  TODAY-DDMMYY	       PIC  9(08) COMP-5.

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

002060 COPY CONTROL.ID.

002120 COPY DEPART.ID.

       COPY PARAM.ID.

002200 COPY SPARTS.ID.

000140 COPY STKMEM.ID.

002220 COPY STOCK.ID.

       COPY TXTRAN.ID.

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
001460				 VALUE "PERIOD END / DATES CONTROL".

       01  S02.
	   03  LINE  4 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
	   03	       COLUMN 29 BACKGROUND-COLOR 2 FOREGROUND-COLOR 0
				 VALUE "¿".
	   03  LINE  5 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "PERIOD END PROGRAMS".
	   03	       COLUMN 28 BACKGROUND-COLOR 2
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
				 VALUE " Day/Month/Year End³".
001470	   03  LINE  7 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "2".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Update Bulk Stock ³".
001470	   03  LINE  8 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "3".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Bank Deposit Slip ³".
001570	   03  LINE  9 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "4".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Banking Date	   ³".
001500	   03  LINE 10 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "5".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Sales Commission  ³".
001500	   03  LINE 11 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "6".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " POS Process Batch ³".
001500	   03  LINE 12 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "7".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " V.A.T. Report     ³".
001500	   03  LINE 13 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "0".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Exit	           ³".
	   03  LINE 14 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "À".
	   03	       COLUMN  8 BACKGROUND-COLOR 2 FOREGROUND-COLOR 0
				 VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S03.
	   03  LINE  6 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
	   03  LINE  7 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE "³    ".
	   03	       COLUMN 32 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "PERIOD-END OPTIONS".
	   03	       COLUMN 50 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE "    ³".
	   03  LINE  8 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "1".
	   03	       COLUMN 30 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Day-End Reports        ³".
	   03  LINE  9 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "2".
	   03	       COLUMN 30 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Month-End Clear        ³".
001470	   03  LINE 10 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "3".
	   03	       COLUMN 30 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Year-End Clear         ³".
001470	   03  LINE 11 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "4".
	   03	       COLUMN 30 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Sales Analysis         ³".
001470	   03  LINE 12 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "5".
	   03	       COLUMN 30 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Category Analysis      ³".
001470	   03  LINE 13 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "6".
	   03	       COLUMN 30 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Purchases Analysis     ³".
001470	   03  LINE 14 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "7".
	   03	       COLUMN 30 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Cost-of-Sales Analysis ³".
001470	   03  LINE 15 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "8".
	   03	       COLUMN 30 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Stock Category Totals  ³".
001470	   03  LINE 16 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "A".
	   03	       COLUMN 30 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Stock Gross Profit     ³".
001470	   03  LINE 17 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "B".
	   03	       COLUMN 30 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Turnover and GP Report ³".
001470	   03  LINE 18 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "C".
	   03	       COLUMN 30 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Sales Staff Analysis   ³".
001470	   03  LINE 19 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "D".
	   03	       COLUMN 30 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Value Added Tax Report ³".
001570	   03  LINE 20 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "0".
	   03	       COLUMN 30 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE " Exit                   ³".
	   03  LINE 21 COLUMN 27 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 0
				 VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       COPY ERROR.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
       AA000	       SECTION.
       AA00.
	   IF LS-DSKDRV = "L"
	       MOVE "Not available on Local drive"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA99.
003240	   IF (LS0-DBLEV < 2) OR (LS0-STLEV < 2) OR
	      (LS0-CRLEV < 2)
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA99.
	     PERFORM ZA000-INIT.
	     DISPLAY CLR-L1-2.
	     DISPLAY S01.
	     PERFORM BA000.
	   IF MOUSE-ATTACHED = "Y"
	       MOVE 66		 TO MOUSE-FUNC
	       MOVE 0		 TO MOUSE-PARAM
	       CALL X"AF" USING MOUSE-FUNC
				MOUSE-PARAM.
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

       COPY FUNCTION.SCR.

       COPY ERROR.SCR.

       COPY LOCKED.RC2.

011433 COPY CLEAR.CRT.

      /
      *    ****    READ FILES ROUTINES
      *
008680 AC000              SECTION.

       COPY PARAM.RD.

       BA000	       SECTION 50.
       BA00.
	     MOVE 5		 TO SHADE-ROW.
	     MOVE 9		 TO SHADE-COL.
	     MOVE 21		 TO SHADE-WIDTH.
	     MOVE 10		 TO SHADE-LINES.
	     DISPLAY S02.
	   IF (LS0-LEV < 2) OR (LS0-POS = 0)
	       DISPLAY "Bulk Stock (N/A) " AT 0711
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2.
	   IF LS0-LEV < 4
	       DISPLAY "(N/A)" AT 0824
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
	       DISPLAY "(N/A)" AT 0924
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2.
	   IF (LS0-LEV < 4) AND (LS0-POS = 0)
	       DISPLAY "Process Batch (N/A)" AT 1111
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.

       BA02.
	     MOVE 5		 TO TOP-ROW.
	     MOVE 12		 TO BOTTOM-ROW.
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
	   IF SCREEN-LIN = 12
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
	       PERFORM CA000
	   ELSE
	   IF WS-OPTION = "2"
	       PERFORM GA000
	   ELSE
	   IF WS-OPTION = "3"
	       PERFORM DA000
	   ELSE
	   IF WS-OPTION = "4"
	       PERFORM EA000
	   ELSE
	   IF WS-OPTION = "5"
	       PERFORM FA000
	   ELSE
	   IF WS-OPTION = "6"
	       PERFORM HA000
	   ELSE
	   IF WS-OPTION = "7"
	       PERFORM IA000.
	     PERFORM RESTORE-SCREEN.
	     MOVE BLOCK-DETAIL1  TO BLOCK-DETAIL.
	     MOVE SHADOW-DETAIL1 TO SHADOW-DETAIL.
	     MOVE CRT-DETAIL1	 TO CRT-DETAIL.
	     GO TO BA05.
       BA999.
	     EXIT.

       CA000	       SECTION 50.
       CA00.
013040	     CLOSE NETWORK
013050		   DEPART
		   PARAM
		   TXTRAN.
      *
      *      THE FOLLOWING FOUR LINES WILL BE DELETED ONCE THE PERIOD
      *      END PROGRAM HAS BEEN SPLIT.
      *
	     MOVE "UTP\REP001"	 TO PRG-NAME.
	     PERFORM AA100.
	     DISPLAY MENU-INSTRUCT.
	     GO TO CA99.

       CA01.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 29		 TO SHADE-COL.
	     MOVE 26		 TO SHADE-WIDTH.
	     MOVE 15		 TO SHADE-LINES.
	     DISPLAY S03.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.

       CA02.
	     MOVE 7		 TO TOP-ROW.
	     MOVE 19		 TO BOTTOM-ROW.
	     MOVE 6		 TO SCREEN-LIN.
	     MOVE 29		 TO SCREEN-COL.
	     MOVE 24		 TO STRING-LENGTH.
	     PERFORM SAVE-ATTR.

       CA05.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

006660 CA10.
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
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
	       IF (WS-OPTION < "0" OR > "8") AND
		  (WS-OPTION < "A" OR > "D")
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
	   GO TO CA18.

       CA16.
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
	       MOVE 18		 TO SCREEN-LIN.
	     PERFORM SAVE-ATTR.
	     PERFORM MARK-BLOCK.

       CA18.
006730	   IF WS-OPTION = "0"
006740	       GO TO CA99.
	     PERFORM SAVE-SCREEN-2.
	     MOVE BLOCK-DETAIL	 TO BLOCK-DETAIL2.
	     MOVE SHADOW-DETAIL	 TO SHADOW-DETAIL2.
	     MOVE CRT-DETAIL	 TO CRT-DETAIL2.
006750	   IF WS-OPTION = "1"
006760	       MOVE "REP\REP001" TO PRG-NAME
006770     ELSE
006780     IF WS-OPTION = "2"
006760	       MOVE "REP\REP002" TO PRG-NAME
006800	   ELSE
006810     IF WS-OPTION = "3"
006760	       MOVE "REP\REP003" TO PRG-NAME
006830	   ELSE
006840     IF WS-OPTION = "4"
006760	       MOVE "REP\REP004" TO PRG-NAME
006860	   ELSE
006870     IF WS-OPTION = "5"
006760	       MOVE "REP\REP005" TO PRG-NAME
006890	   ELSE
006900     IF WS-OPTION = "6"
006760	       MOVE "REP\REP006" TO PRG-NAME
006920	   ELSE
006930     IF WS-OPTION = "7"
006760	       MOVE "REP\REP007" TO PRG-NAME
006950	   ELSE
006960     IF WS-OPTION = "8"
006760	       MOVE "REP\REP008" TO PRG-NAME
006980	   ELSE
006990     IF WS-OPTION = "A"
006760	       MOVE "REP\REP009" TO PRG-NAME
007010	   ELSE
006990     IF WS-OPTION = "B"
006760	       MOVE "REP\REP010" TO PRG-NAME
007010	   ELSE
007020	   IF WS-OPTION = "C"
006760	       MOVE "REP\REP011" TO PRG-NAME
007040	   ELSE
007020	   IF WS-OPTION = "D"
006760	       MOVE "REP\REP012" TO PRG-NAME.
	     PERFORM AA100.
	     DISPLAY MENU-INSTRUCT.
	     GO TO CA05.

       CA99.
013040	     OPEN I-O NETWORK
013050		      DEPART
		      PARAM
		      TXTRAN.
       CA999.
	     EXIT.

       DA000	       SECTION 51.
       DA00.
	   IF LS0-LEV < 4
	       MOVE "MNU\NOPMENU"
				 TO PRG-NAME
	   ELSE
	       MOVE "UTP\BNK001" TO PRG-NAME.
	     PERFORM AA100.
	     DISPLAY MENU-INSTRUCT.

       DA999.
	     EXIT.
       EA000	       SECTION 52.
       EA00.
	   IF LS0-LEV < 4
	       MOVE "MNU\NOPMENU"
				 TO PRG-NAME
	   ELSE
	       MOVE "UTP\BNK002" TO PRG-NAME.
	     PERFORM AA100.
	     DISPLAY MENU-INSTRUCT.

       EA999.
	     EXIT.

       FA000	       SECTION 53.
       FA00.
013040	     CLOSE NETWORK
		   PARAM.
	     MOVE "UTP\SLSCOM"	 TO PRG-NAME.
	     PERFORM AA100.
	     DISPLAY MENU-INSTRUCT.
013040	     OPEN I-O NETWORK
		      PARAM.

       FA999.
	     EXIT.
      /
       GA000	       SECTION 54.
       GA00.
	   IF (LS0-LEV < 2) OR (LS0-POS = 0)
	       MOVE "MNU\NOPMENU"
				 TO PRG-NAME
	   ELSE
	       MOVE "UTP\STPBLK" TO PRG-NAME.
	     PERFORM AA100.
	     DISPLAY MENU-INSTRUCT.

       GA999.
	     EXIT.
      /
       HA000	       SECTION 55.
       HA00.
	   IF (LS0-LEV < 4) AND (LS0-POS = 0)
	       MOVE "MNU\NOPMENU"
				 TO PRG-NAME
	   ELSE
	       MOVE "POS\POSUPD" TO PRG-NAME.
	     PERFORM AA100.
	     DISPLAY MENU-INSTRUCT.

       HA999.
	     EXIT.
      /
       IA000	       SECTION 56.
       IA00.
	   IF (LS0-LEV < 4) AND (LS0-POS = 0)
	       MOVE "MNU\NOPMENU"
				 TO PRG-NAME
	   ELSE
	       MOVE "UTP\VATREP" TO PRG-NAME.
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
038300	     MOVE 3		 TO WS-PARKEY.
038310	     PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      *
      *    ****    G E T   F I L E   P A T H S	 A N D	 I D ' S
      *
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
	     MOVE "DEPART"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEPART.
	     MOVE "TXTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-TXTRAN.
039770	     GO TO ZA999.

039780 ZA49.
039790       DISPLAY "Too many files OPEN" AT 0812
039800                WITH FOREGROUND-COLOR 14.
039810       DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
039820       DISPLAY "Press any key to continue" AT 1212.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
039840	     GO TO ZA205.

039850 ZA50.
039860	   IF WS-F-ERROR = 32 OR 35
039870	       MOVE "TAX file"	 TO WS-FILE.
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
	       MOVE "SupervisorÄ"
				 TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"
				 TO WS-WRKHD
	       MOVE LS-USER	 TO WS-WRKST.

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
041120*	   IF WS-F-ERROR = 15
041130*        MOVE WS-PARID	 TO WS-FILE
041140*        MOVE WS-PARKEY	 TO WS-KEY.
041350	   IF WS-STATUS = "10"
041360	       MOVE "End of FILE"
				 TO WS-STAT-MESSAGE
041370     ELSE
041380     IF WS-STATUS = "22"
041390	       MOVE "Duplicate record number"
				 TO WS-STAT-MESSAGE
041400     ELSE
041410     IF WS-STATUS = "23"
041420	       MOVE "Invalid record number"
				 TO WS-STAT-MESSAGE
041430     ELSE
041440     IF WS-STATUS = "24"
041450	       MOVE "DISK full"  TO WS-STAT-MESSAGE
041460     ELSE
041470     IF WS-STATUS = "30"
041480         MOVE "DISK error" TO WS-STAT-MESSAGE
041490     ELSE
041500     IF WS-STATUS = "94"
041510	       MOVE "FILE locked"
				 TO WS-STAT-MESSAGE.
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
