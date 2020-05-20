      $set LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *    ******   ********  *******	  ****	  ********     **      *
      *   **	**     **     **   **	 **  **   **	      ***      *
      *   **	       **     **   **	**    **  *******      **      *
      *    ******      **     ******	**    **	**     **      *
      * 	**     **     **	**    **	**     **      *
      *   **	**     **     **	 **  **   **	**     **      *
      *    ******      **     **	  ****	   ******   ********   *
      * 							       *
      *     ENGLISH                                                    *
      *                                                                *
      *     S T O C K	E X C E P T I O N S   R E P O R T   P R O G    *
      * 							       *
      *     VERSION 8.13.07 - March 2008			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     STP051.
000030 AUTHOR.         J.W.LEMMON (APAC Software).
000040 DATE-WRITTEN.   SEPTEMBER 1989.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1989 - 2008
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
000080		       CURSOR IS CSTART
000090		       CONSOLE IS CRT
		       CRT STATUS IS KEY-STATUS.
000090 INPUT-OUTPUT SECTION.
000100 FILE-CONTROL.

       COPY APACFIDS.SL.

       COPY CONTROL.SL.

       COPY DEPART.SL.

000120 COPY PARAM.SL.

000140 COPY STOCK.SL.

      /
000260 DATA DIVISION.
000270 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CONTROL.FDE.

       COPY DEPART.FDE.

000290 COPY PARAM.FDE.

000310 COPY STOCK.FDE.

      /
003090 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
003100 77  WS-S1	   PIC	9(04)	 COMP-5.
003110 77  WS-S2	   PIC	9(04)	 COMP-5.
003120 77  WS-S3	   PIC	9(04)	 COMP-5.
003130 77  WS-S4	   PIC	9(04)	 COMP-5.
003140 77  WS-S5	   PIC	9(04)	 COMP-5.
003140 77  WS-S6	   PIC	9(04)	 COMP-5.
003140 77  WS-S7	   PIC	9(04)	 COMP-5.
003140 77  WS-S8	   PIC	9(04)	 COMP-5.
003150 77  WS-IXS	   PIC	9(04)	 COMP-5.
003150 77  WS-IXS1	   PIC	9(04)	 COMP-5.
003160 77  WS-IXS2	   PIC	9(04)	 COMP-5.
003170 77  WS-IXS3	   PIC	9(04)	 COMP-5.
003180 77  WS-IXS4	   PIC	9(04)	 COMP-5.
003230 77  WS-PARKEY	   PIC	9(04)	 COMP-5.
003250 77  WS-NETKEY	   PIC	9(04)	 COMP-5.
003270 77  WS-PAGE	   PIC	9(04)	 COMP-5.
       77  PRG-PRINTUTL	   PIC	X(12) VALUE "UTP\PRINTUTL".
003300 77  WS-PQNT	   PIC	X(01).
003310 77  WS-WS	   PIC	X(01) VALUE "N".
003310 77  WS-MEMO	   PIC	X(01) VALUE "N".
003310 77  WS-PRT	   PIC	X(01).
003320 77  WS-BIN	   PIC	X(01).
       77  WS-ETYPE	   PIC	X(01).
003330 77  WS-OPTION	   PIC	X(01).
       77  WS-USE-ITM	   PIC	X(01).
       77  WS-EXT-STK	   PIC	X(01).
003330 77  WS-PRICE	   PIC	X(01).
003330 77  WS-CHK	   PIC	X(01) VALUE "N".
003350 77  WS-ERROR	   PIC	9(01).
003470 77  WS-ORD	   PIC S9(05)	 COMP-3.
003480 77  WS-TOTAL	   PIC S9(07)V99 COMP-3.
003490 77  WS-VALUE	   PIC S9(07)V99 COMP-3.
003500 77  WS-CCOST	   PIC S9(07)V99 COMP-3.
003510 77  WS-CSELL	   PIC S9(07)V99 COMP-3.
003520 77  WS-SCOST	   PIC S9(07)V99 COMP-3.
003530 77  WS-SSELL	   PIC S9(07)V99 COMP-3.
003540 77  WS-TCOST	   PIC S9(07)V99 COMP-3.
003550 77  WS-TSELL	   PIC S9(07)V99 COMP-3.
003560 77  WS-DEPT	   PIC	X(04).
003570 77  WS-WEEK	   PIC	9(01).
001330 77  WS-DESC-H1	   PIC	X(14) VALUE "Description".
       77  WS-DESC-H2	   PIC	X(14) VALUE "Description 2".
       77  WS-RT-HD	   PIC	X(10) VALUE "-Retail   ".
       77  WS-WS-HD	   PIC	X(10) VALUE "-Wholesale".
       77  WS-CS-HD	   PIC	X(10) VALUE "-Cash sale".
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.
      /
002420 01  WS-DB-LINE.
002430     03  WS-TOP-LNE.
002440         05  WS-TCHR PIC X(01) OCCURS 80.
           03  WS-T-LINE REDEFINES WS-TOP-LNE.
               05  FILLER  PIC X(01).
               05  WS-H-LINE
                           PIC X(78).
               05  FILLER  PIC X(01).
002430     03  WS-TOP-LNE2.
002440         05  WS-TCH  PIC X(01) OCCURS 80.
           03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
               05  FILLER      PIC  X(01).
               05  WS-TOP-COMP PIC  X(40).
001430	       05  FILLER      PIC  X(23).
	       05  WS-WRKHD    PIC  X(11).
001430	       05  FILLER      PIC  X(01).
	       05  WS-WRKST    PIC  X(03).
001430	       05  FILLER      PIC  X(01).
002450	   03  WS-MID-LNE.
002460         05  WS-MCHR PIC X(01) OCCURS 80.
	   03  WS-MST-LNE REDEFINES WS-MID-LNE.
	       05  WS-MST1     PIC  X(01).
	       05  WS-MST78    PIC  X(78).
	       05  WS-MST80    PIC  X(01).
002450	   03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "³".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "°".
	       05  FILLER      PIC  X(01) VALUE "³".
002470	   03  WS-BOT-LNE.
002480         05  WS-BCHR PIC X(01) OCCURS 80.
           03  WS-B-LINE REDEFINES WS-BOT-LNE.
               05  FILLER  PIC X(01).
               05  WS-F-LINE
                           PIC X(78).
               05  FILLER  PIC X(01).
002470     03  WS-BOT-LNE2.
002480         05  WS-BCH  PIC X(01) OCCURS 80.
003240     03  WS-B-LINE REDEFINES WS-BOT-LNE2.
003250         05  FILLER  PIC X(01).
               05  WS-MEM-LINE.
003260            07  WS-SRT-LINE
003270                     PIC X(50).
                  07  FILLER
                           PIC X(10).
003280         05  FILLER  PIC X(19).
           03  WS-MM-LINE.
               05  FILLER  PIC  X(01) VALUE X"B3".
               05  FILLER  PIC  X(60) VALUE SPACES.
               05  FILLER  PIC  X(01) VALUE X"B3".
           03  WS-HME-LINE PIC  X(60) VALUE ALL X"CD".

003590 COPY WS.WS.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

003610 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

       COPY DEPART.ID.

       COPY PARAM.ID.

000300 COPY STOCK.ID.

       01  W02-PRINTER-DETAILS.
	   03  W02-PRINTER     PIC  X(12).
002570	   03  W02-PGE-LENGTH  PIC  9(02).
001870	   03  W02-PRN-LENGTH  PIC  9(02).
	   03  W02-LINAGE      PIC  9(02).
	   03  W02-PRN-STATUS  PIC  X(01) VALUE "C".
      *
      *    ****    D  =  Detail line
      * 	   C  =  Condensed print
      * 	   E  =  Expanded print
      * 	   H  =  Header line
      * 	   X  =  Cancel expanded print
      * 	   1  =  10 Characters per inch
      * 	   2  =  12 Characters per inch
      * 	   3  =  17 Characters per inch
      * 	   6  =  6 Lines per inch
      * 	   8  =  8 Lines per inch
      *
	   03  W02-PRN-TYPE    PIC  X(01).
	   03  W02-PRN-LINE    PIC  X(136).

	   03  R-SL1 REDEFINES W02-PRN-LINE.
	       05  R-SLA       PIC  X(78).
	       05  FILLER      PIC  X(58).

	   03  R-SL2 REDEFINES W02-PRN-LINE.
	       05  FILLER      PIC  X(58).
	       05  R-SLB       PIC  X(78).

000390	   03  REP-LINE1 REDEFINES W02-PRN-LINE.
000400	       05  REP-DETAIL1 PIC  X(140).

000410	   03  REP-LINE2 REDEFINES W02-PRN-LINE.
000420	       05  REP-DETAIL2 PIC  X(80).

000430	   03  REP-LINE3 REDEFINES W02-PRN-LINE.
000490	       05  REP-DATE    PIC  X(06).
000500	       05  REP-DTE     PIC  Z9/99/9999.
000460	       05  FILLER      PIC  X(03).
	       05  REP-PRC-HD.
		   07  FILLER  PIC  X(01).
000470		   07  REP-COMPANY  PIC	X(40).
		   07  FILLER  PIC  X(02).
000480	       05  FILLER      PIC  X(08).
000440	       05  REP-PAGE    PIC  X(06).
000450	       05  REP-P-NO    PIC  Z(03)9.

000510	   03  REP-LINE4 REDEFINES W02-PRN-LINE.
001090	       05  EXC-H1      PIC  X(19).
001100	       05  EXC-H2      PIC  X(31).
	       05  EXC-H3      PIC  X(46).

	   03  REP-LINE5 REDEFINES W02-PRN-LINE.
	       05  EXC-EXT-ITEM.
		   07  EXC-ITEM
			       PIC  X(14).
		   07  EXC-SL  PIC  X(01).
		   07  EXC-ITM PIC  X(03).
	       05  FILLER      PIC  X(01).
	       05  EXC-DESC    PIC  X(30).
	       05  FILLER      PIC  X(01).
	       05  EXC-REMKS.
		   07  FILLER	   PIC	X(26).
		   07  EXC-MARKUP  PIC	ZZ9.9999.
		   07  FILLER	   PIC	X(12).

003740 01  W05-SALES-LEDGERS.
	   03  W05-SL OCCURS 300.
003750	       05  W05-S      PIC 9(04)	   COMP-5.

       COPY W05.VAT.

       COPY W10.STK.

       COPY W12.WS.

       01  W15-LABELS.
           03  W15-DEPTH          OCCURS 2.
               05  W15-WIDTH      OCCURS 3.
                   07  W15-ITM    PIC  X(14).
                   07  W15-DESC   PIC  X(30).
                   07  W15-PRC    PIC  9(05)V99.
       01  W15-LAB-2 REDEFINES W15-LABELS.
           03  W15-LABS OCCURS 4.
               05  W15-ITEM       PIC  X(14).
               05  W15-DSC        PIC  X(22).
               05  W15-PRICE      PIC  9(05)V99.
       01  W20-LABEL.
           03  W20-CODE           PIC  X(14).
           03  W20-DESC           PIC  X(30).
           03  W20-COST.
               05  W20-CST        PIC  9(05)V99.
           03  W20-SELL           PIC  9(05)V99.
           03  W20-NO             PIC  9(05)    COMP-0.
004310 01  W25-CALCS.
005040     03  W25-RESULT      PIC 9(05)V99.
004330     03  W25-RESULT1 REDEFINES W25-RESULT.
004340         05  W25-WHOLE   PIC 9(05).
004350         05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY     PIC 9(04).
               05  W25-SUB     PIC 9(01).
               05  FILLER      PIC 9(02).
003790     03  W25-VALUE          PIC S9(07)V9(04) COMP-3.
003800     03  W25-TOTAL          PIC S9(07)V9(04) COMP-3.
003810 01  W30-COST-CODE.
003820     03  W30-CODE           PIC X(01) OCCURS 10.
003830 01  W35-COST.
003840     03  W35-CST            PIC 9(08)V99.
003850     03  W35-WHOLE  REDEFINES W35-CST
                                  PIC 9(10).
003860     03  W35-CPRINT.
003870         05  W35-CEDIT      PIC ZZZZZZ9999.
003880     03  W35-EOC REDEFINES W35-CPRINT.
003890         05  W35-CHAR       PIC X(01) OCCURS 10.
003900     03  W35-EON REDEFINES W35-CPRINT.
003910         05  W35-NUM        PIC 9(01) OCCURS 10.
003920     03  W35-S1             PIC 9(02) COMP-5.
003930     03  W35-S2             PIC 9(02) COMP-5.

       01  W40-DATES.
	   03  W40-DATE1	  PIC  9(08).
           03  W40-DTE1 REDEFINES W40-DATE1.
	       05  W40-C1	  PIC  9(02).
	       05  W40-Y1	  PIC  9(02).
	       05  W40-M1	  PIC  9(02).
	       05  W40-D1	  PIC  9(02).
	   03  W40-DATE2	  PIC  9(08).
           03  W40-DTE2 REDEFINES W40-DATE2.
	       05  W40-C2	  PIC  9(02).
	       05  W40-Y2	  PIC  9(02).
	       05  W40-M2	  PIC  9(02).
	       05  W40-D2	  PIC  9(02).

       COPY W40.WS.

       COPY FUNCTION.WS.

003950 COPY W50.WS.

004040 COPY W70STOCK.WS.

004060 01  W70-KEYS.
004070     03  W70-BIND        PIC 9(05)     COMP-0 VALUE ZERO.

       COPY W75.STK.

004080 01  W95-COMP	       PIC X(40).

005120 01  W100-EDIT.
003970     03  W100-V2.
003980         05  W100-S6V2   PIC Z(05)9.99-.
           03  W100-V3 REDEFINES W100-V2.
               05  W100-PRICE  PIC X(09).
               05  FILLER      PIC X(01).
           03  W100-V5 REDEFINES W100-V2.
               05  W100-QNT    PIC Z(08)9-.

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
004090 SCREEN SECTION.

       COPY BLANK.CRT.

004440 01  S03.
004450	   03  LINE 18 COLUMN 26 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
004460                           PIC X(14) USING STK-CODE.

004470 01  S04.
004480	   03  LINE 10 COLUMN 20 FOREGROUND-COLOR 14
004490                           VALUE "Busy loading stock departments".

004820 01  S13.
004830	   03  LINE  2 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
004840				 VALUE "STOCK EXCEPTIONS REPORT".
004850	   03  LINE  4 COLUMN 12 FOREGROUND-COLOR 6 HIGHLIGHT VALUE
004860				 "Busy printing Stock Exception report".
004870	   03  LINE  6 COLUMN 12 VALUE "Are you sure (".
	   03	       COLUMN 26 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Y".
	   03	       COLUMN 27 VALUE "/".
	   03	       COLUMN 28 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "N".
	   03	       COLUMN 29 VALUE ")?".

005040 01  S15.
005050	   03  LINE 18 COLUMN 12 VALUE "Stock record:".

       COPY ERROR.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
005070 AA00M		 SECTION.
005080 AA00.
003240	   IF LS0-STLEV < 3
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
005090	     PERFORM ZA000-INIT.
	     PERFORM BE000.
	   IF WS-ERROR = 9
	       MOVE "No details in Department file"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
      *
      *  ***  ****  ***** *   *   ****	****  *** *   * ***** ***** ****
      * *   * *   * *	  **  *   *   * *   *  *  **  *   *   *     *	*
      * *   * ****  ***   * * *   ****	****   *  * * *   *   ***   ****
      * *   * *     *	  *  **   *	*   *  *  *  **   *   *     *	*
      *  ***  *     ***** *   *   *	*   * *** *   *   *   ***** *	*
      *
	     PERFORM OPEN-PRINTER.
	     MOVE "P"		 TO WS-COMMAND.
	     PERFORM EE000.
      *
      *    ****   1 0	C P I	( N O R M A L	P R I N T )
      *
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 12		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL
012960	       MOVE SPACES	 TO REP-DETAIL1.
	     MOVE "C"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.

       AA49.
             EXIT PROGRAM.

       COPY FUNCTION.SCR.

       COPY ERROR.SCR.

       COPY LOCKED.RC2.

       COPY CLEAR.CRT.

       COPY ABORT.PRN.

       COPY PRINTUTL.AP3.

      /
      *    ****    READ FILES ROUTINES
      *
008180 AC000-READ              SECTION.

       COPY CONTROL.RD.

       COPY DEPART.RD.

       COPY PARAM.RD.

       COPY STOCK.RD.

       COPY STOCK.1ST.

      /
008500 BE000		SECTION.
008510 BE00.
	     MOVE "!"		 TO DPT-CODE.
	     PERFORM START-AT-DEPT-CODE THRU READ-DEPART-EXIT.
	   IF WS-F-ERROR = 7
	       MOVE 9		 TO WS-ERROR
	   ELSE
	       MOVE ZERO	 TO WS-ERROR.

       BE09.
	     EXIT.
      /
016160 EE000	       SECTION 5.
016170 EE00.
016180       DISPLAY CLR-SCREEN.
016190       DISPLAY S13.
016210	     MOVE "Y"		 TO WS-OPTION.

016220 EE02.
016230	     ACCEPT WS-OPTION AT 0632
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
016240			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
016300	   IF NOT (WS-OPTION = "Y" OR "N")
016310         GO TO EE02.
016320     IF WS-OPTION = "N"
016330         GO TO EE999.
	     PERFORM EE50.

       EE05.
	     DISPLAY S15.

016540 EE10.
016550	     MOVE 0		 TO WS-S1.
	     PERFORM READ-DEPART-NEXT THRU READ-DEPART-EXIT.
	   IF WS-F-ERROR = 7
	       GO TO EE45.
	   IF (DPT-CODE = "XXXX" OR "YYYY") OR
	      (DPT-SCDE = "VAT")
		GO TO EE10.

       EE12.
	   IF WS-CHK = "Y"
	       IF NOT (DPT-CODE = STK-LDG)
		   IF DPT-CODE < STK-LDG
		       GO TO EE10
		   ELSE
		       MOVE "N"	 TO WS-CHK
		       MOVE STK-LDG
				 TO WS-DEPT
		       PERFORM EE30 THRU EE40
		       GO TO EE12.
	   IF DPT-MARKUP = ZERO
	       MOVE 5.00	 TO DPT-MARKUP.
	   IF DPT-CMRKUP = ZERO
	       MOVE 25.00	 TO DPT-MARKUP.
	   IF DPT-WMRKUP = ZERO
	       MOVE 15.00	 TO DPT-MARKUP.
	     MOVE DPT-CODE	 TO STK-LDG.
	     MOVE "!"		 TO STK-CODE.
             PERFORM START-AT-STOCK-LEDG THRU READ-STOCK-EXIT.
016630     IF WS-F-ERROR = 22
016640	       GO TO EE10.

016650 EE15.
             PERFORM READ-STOCK-NEXT THRU READ-STOCK-EXIT.
016660	   IF WS-F-ERROR = 22
	       MOVE "N" 	 TO WS-CHK
016670	       GO TO EE10.
	   IF DPT-CODE NOT = STK-LDG
	       MOVE "Y" 	 TO WS-CHK
016670	       GO TO EE10.
	   IF STK-IND = 4
	       GO TO EE15.
	   IF NOT (WS-PRINTER = "S")
016710	       DISPLAY S03.
      *    ****    A V E R A G E   C O S T
	   IF STK-QUANT < 0.001
	       MOVE STK-COST	 TO STK-AVGE.
	     COMPUTE W10-MARKUP ROUNDED = (STK-SELL - STK-COST) /
					   STK-COST * 100.0000.
	     COMPUTE W10-CMARKUP ROUNDED = (STK-CASH - STK-COST) /
					    STK-COST * 100.0000.
	     COMPUTE W10-WMARKUP ROUNDED = (STK-WSALE - STK-COST) /
					    STK-COST * 100.0000.
	   IF WS-USE-ITM = "Y"
	       MOVE STK-ITEM	 TO EXC-ITEM
	       MOVE "/" 	 TO EXC-SL
	       MOVE STK-ITM	 TO EXC-ITM
	   ELSE
015280	       MOVE STK-CODE	 TO EXC-EXT-ITEM.
016780	     MOVE STK-DESC	 TO EXC-DESC.
	   IF STK-IND = 2
	       GO TO EE15.
	   IF STK-COST = ZERO
	       MOVE "Cost price is ZERO"
				 TO EXC-REMKS
	       PERFORM EE20
	       GO TO EE15.
	   IF W10-MARKUP < (DPT-MARKUP - 2)
	       MOVE SPACES	 TO EXC-REMKS
	       STRING WS-RT-HD DELIMITED SIZE
		      " markup < S.Ledg" DELIMITED SIZE
				 INTO EXC-REMKS
	       MOVE W10-MARKUP	 TO EXC-MARKUP
	       PERFORM EE20.
	   IF W10-CMARKUP < (DPT-CMRKUP - 2)
	       MOVE SPACES	 TO EXC-REMKS
	       STRING WS-CS-HD DELIMITED SIZE
		      " markup < S.Ledg" DELIMITED SIZE
				 INTO EXC-REMKS
	       MOVE W10-CMARKUP	 TO EXC-MARKUP
	       PERFORM EE20.
	   IF W10-WMARKUP < (DPT-WMRKUP - 2)
	       MOVE SPACES	 TO EXC-REMKS
	       STRING WS-WS-HD DELIMITED SIZE
		      " markup < S.Ledg" DELIMITED SIZE
				 INTO EXC-REMKS
	       MOVE W10-WMARKUP	 TO EXC-MARKUP
	       PERFORM EE20.
	   IF W10-MARKUP > 350.00
	       MOVE SPACES	 TO EXC-REMKS
	       STRING WS-RT-HD DELIMITED SIZE
		      " markup > 350%" DELIMITED SIZE
				 INTO EXC-REMKS
	       MOVE W10-MARKUP	 TO EXC-MARKUP
	       PERFORM EE20.
	   IF W10-CMARKUP > 350.00
	       MOVE SPACES	 TO EXC-REMKS
	       STRING WS-CS-HD DELIMITED SIZE
		      " markup > 350%" DELIMITED SIZE
				 INTO EXC-REMKS
	       MOVE W10-CMARKUP	 TO EXC-MARKUP
	       PERFORM EE20.
	   IF W10-WMARKUP > 350.00
	       MOVE SPACES	 TO EXC-REMKS
	       STRING WS-WS-HD DELIMITED SIZE
		      " markup > 350%" DELIMITED SIZE
				 INTO EXC-REMKS
	       MOVE W10-WMARKUP	 TO EXC-MARKUP
	       PERFORM EE20.
017010       GO TO EE15.

       EE20.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
016970	       PERFORM EE50.
016980       MOVE SPACES TO REP-DETAIL1.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO REP-DETAIL1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
014990	       GO TO EE999.

016990 EE22.
	     EXIT.

       EE30.
	   IF STK-IND = 2 OR 4
	       GO TO EE35.
	   IF WS-USE-ITM = "Y"
	       MOVE STK-ITEM	 TO EXC-ITEM
	       MOVE "/" 	 TO EXC-SL
	       MOVE STK-ITM	 TO EXC-ITM
	   ELSE
015280	       MOVE STK-CODE	 TO EXC-EXT-ITEM.
016780	     MOVE STK-DESC	 TO EXC-DESC.
	     MOVE "Invalid sales ledger code"
				 TO EXC-REMKS.
	     PERFORM EE20.
	   IF STK-IND = 2
	       GO TO EE35.
	   IF STK-COST = ZERO
	       MOVE "Cost price is ZERO"
				 TO EXC-REMKS
	       PERFORM EE20.

       EE35.
	     PERFORM READ-STOCK-NEXT THRU READ-STOCK-EXIT.
016660	   IF WS-F-ERROR = 22
	       MOVE "N" 	 TO WS-CHK
016670	       GO TO EE40.
	   IF NOT (WS-DEPT = STK-LDG)
	       MOVE "Y" 	 TO WS-CHK
016670	       GO TO EE40.
	   IF STK-IND = 4
	       GO TO EE35.
	   IF NOT (WS-PRINTER = "S")
016710	       DISPLAY S03.
	     GO TO EE30.

       EE40.
	     EXIT.

017340 EE45.
018790	     MOVE SPACES	 TO REP-DETAIL1.
      *
      *    ****   1 0	C P I	( N O R M A L	P R I N T )
      *
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 12		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL
012960	       MOVE SPACES	 TO REP-DETAIL1.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
018810	     MOVE "****** END OF REPORT *****"
				 TO REP-DETAIL1.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     MOVE "P"		 TO WS-COMMAND.
018830	     MOVE SPACES	 TO REP-DETAIL1.
017350	     GO TO EE999.

017360 EE50.
      *
      *    ****   1 0	C P I	( N O R M A L	P R I N T )
      *
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 12		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL
012960	       MOVE SPACES	 TO REP-DETAIL1.
	     MOVE "H"		 TO W02-PRN-TYPE.
017370	     MOVE SPACES	 TO REP-DETAIL1.
017380	     MOVE "Page:"	 TO REP-PAGE.
017390	     MOVE "Date:"	 TO REP-DATE.
017400	     MOVE W95-COMP	 TO REP-COMPANY.
017410	     ADD 1		 TO WS-PAGE.
017420	     MOVE WS-PAGE	 TO REP-P-NO.
017430	     MOVE W12-TODAY	 TO REP-DTE.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
017460	     MOVE SPACES	 TO REP-DETAIL1.
017440	     MOVE "STOCK FILE - EXCEPTIONS REPORT"
				 TO REP-COMPANY.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
017460	     MOVE SPACES	 TO REP-DETAIL1.
      *
      *    ****   1 2	C P I	( E L I T E   P R I N T )
      *
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 2		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL
012960	       MOVE SPACES	 TO REP-DETAIL1
	       MOVE "H" 	 TO W02-PRN-TYPE.
017520	     MOVE "Item Code"	 TO EXC-H1.
017590	     MOVE WS-DESC-H1	 TO EXC-H2.
017600	     MOVE "Remarks"	 TO EXC-H3.
	     PERFORM CALL-PRINTUTL.
017660	     MOVE SPACES	 TO REP-DETAIL1.

017670 EE999.
017680       EXIT.
      /
019370 ZA000-INIT	       SECTION 8.
019380 ZA000-OPEN.
             PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
019390	     DISPLAY CLR-SCREEN.
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
	       MOVE 66		 TO MOUSE-FUNC
	       MOVE 0		 TO MOUSE-PARAM
	       CALL X"AF" USING MOUSE-FUNC
				MOUSE-PARAM
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
	     MOVE "DEPART"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEPART.
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
	     MOVE "STOCKF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STOCKF.
019480	     MOVE 3		 TO WS-PARKEY.
044240       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE PAR-USE-ITM	 TO WS-USE-ITM.
	     MOVE PAR-EXT-STK	 TO WS-EXT-STK.

       ZA01B.
	     MOVE 6		 TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE PAR-WHOLESALE  TO WS-WS.
019880	     MOVE PAR-COST-CODE  TO W30-COST-CODE.
	     MOVE 1		 TO W05-V-RATE.

       ZA05-VAT.
	     MOVE W05-VAT-CODE	 TO DPT-CODE.
	     PERFORM READ-DEPART THRU READ-DEPART-EXIT.
	     MOVE DPT-R-DATE	 TO WS-VAT-DATE(W05-V-RATE).
	     MOVE DPT-RATE	 TO W05-VAT(W05-V-RATE).
	     ADD 6 W05-V-RATE	 GIVING WS-S1.
	     MOVE DPT-P-RATE	 TO W05-VAT(WS-S1).
	   IF W05-V-RATE < 6
	       ADD 1		 TO W05-V-RATE
	       GO TO ZA05-VAT.
019620	     MOVE 3		 TO WS-NETKEY.
019630       PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
019690	     MOVE STK-WEEK	 TO WS-WEEK.
042520	     MOVE 8		 TO WS-PARKEY.
042550       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	   IF NOT (PAR-DESC-H1 = X"0000000000000000000000000000")
	       MOVE PAR-DESC-H1  TO WS-DESC-H1.
	   IF NOT (PAR-DESC-H2 = X"0000000000000000000000000000")
	       MOVE PAR-DESC-H2  TO WS-DESC-H2.
	   IF NOT (PAR-WS-HD = X"00000000000000000000")
	       MOVE PAR-WS-HD	 TO WS-WS-HD.
	   IF NOT (PAR-CS-HD = X"00000000000000000000")
	       MOVE PAR-CS-HD	 TO WS-CS-HD.
	   IF NOT (PAR-RT-HD = X"00000000000000000000")
	       MOVE PAR-RT-HD	 TO WS-RT-HD.
	     INSPECT WS-RT-HD REPLACING LEADING "-" BY " ".
	     INSPECT WS-CS-HD REPLACING LEADING "-" BY " ".
	     INSPECT WS-WS-HD REPLACING LEADING "-" BY " ".
019820	     MOVE 1		 TO WS-PARKEY.
019830       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
019840       MOVE PAR-DMY        TO W12-TODAY.
             MOVE PAR-YMD        TO W40-DATE1 W40-DATE2.
           IF W40-M2 > 3
               SUBTRACT 3        FROM W40-M2
           ELSE
               SUBTRACT 1        FROM W40-Y2
               ADD 9             TO W40-M2.
019850	     MOVE PAR-COMPANY	 TO W95-COMP.

       ZA05.
018380	     DISPLAY WS-BLNK78 AT 1402 WITH FOREGROUND-COLOR 3.
019900	     GO TO ZA999-EXIT.

055270 ZA49.
055280       DISPLAY "Too many files OPEN" AT 0812
055290                WITH FOREGROUND-COLOR 14.
055300       DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
055310       DISPLAY "Press any key to continue" AT 1212.
055320       ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
055330       GO TO ZA205.

055340 ZA50.
055390     IF WS-F-ERROR = 22
055400	       MOVE "STOCK file" TO WS-FILE.
055440       DISPLAY "Rebuild " AT 0812
055450                WS-FILE WITH FOREGROUND-COLOR 14.
055460       DISPLAY "Press any key to continue" AT 1012.
055470       ACCEPT WS-OPTION AT 1038 WITH FOREGROUND-COLOR 15.

055480 ZA51.
055490       EXIT PROGRAM.
      *
      *    ****    S E T U P   T H E   S C R E E N   G R A P H I C S
      *
       ZA55.
041220       MOVE 1              TO WS-S1.
021870	     MOVE SPACES	 TO WS-MID-LNE.

041240 ZA60.
041250       MOVE WS-G1 TO WS-TCHR (WS-S1) WS-BCHR (WS-S1).
             MOVE WS-G8 TO WS-TCH  (WS-S1) WS-BCH  (WS-S1).
041260     IF WS-S1 < 80
041270         ADD 1 TO WS-S1
041280         GO TO ZA60.
             MOVE WS-G9          TO WS-TCH  (1).
             MOVE WS-G10         TO WS-TCH  (80).
             MOVE WS-G11         TO WS-BCH  (1).
             MOVE WS-G12         TO WS-BCH  (80).
             MOVE WS-G14         TO WS-TCHR (1)  WS-BCHR (1).
             MOVE WS-G13         TO WS-TCHR (80) WS-BCHR (80).
021930       MOVE WS-G2          TO WS-TCHR (18) WS-TCHR (49)
021940                              WS-TCHR (60) WS-TCHR (70).
021950       MOVE WS-G3          TO WS-MCHR (18) WS-MCHR (49)
021960                              WS-MCHR (60) WS-MCHR (70)
				    WS-MCHR (1)  WS-MCHR (80).
021970       MOVE WS-G4          TO WS-BCHR (18) WS-BCHR (49)
021980                              WS-BCHR (60) WS-BCHR (70).
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "SupervisorÄ"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.

019910 ZA200.
019920       DISPLAY "Files locked - Try later" AT 2312
019930                WITH FOREGROUND-COLOR 14
                     " " WS-STATUS WITH FOREGROUND-COLOR 15.
019940       DISPLAY "Press any key" AT 2512 
019950		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
019960	     ACCEPT WS-OPTION AT 2526
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.

       ZA205.
             EXIT PROGRAM.

019990 ZA999-EXIT.
020000       EXIT.
      /
020010 ZB000-ERROR	       SECTION 8.
      
041480 OPEN-ERROR.
041490       DISPLAY CLR-SCREEN.
041500       DISPLAY "Open error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       GO TO DISPLAY-FILE-NAME.
      
041480 READ-ERROR.
041490       DISPLAY CLR-SCREEN.
041500       DISPLAY "Read error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       GO TO DISPLAY-FILE-NAME.
      
041520 WRITE-ERROR.
041530       DISPLAY CLR-SCREEN.
041540       DISPLAY "Write error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       GO TO DISPLAY-FILE-NAME.
      
041560 DISPLAY-FILE-NAME.
041610     IF WS-F-ERROR = 2
041620         MOVE W02-NETWORK  TO WS-FILE
041630         MOVE WS-NETKEY    TO WS-KEY
041800     ELSE
041810     IF WS-F-ERROR = 7
041820	       MOVE W02-DEPART	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
041830	       MOVE DPT-CODE	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 15
041860         MOVE WS-PARID     TO WS-FILE
041870         MOVE WS-PARKEY    TO WS-KEY
041840	   ELSE
041850     IF WS-F-ERROR = 22
041860         MOVE W02-STOCKF   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE STK-CODE	 TO WS-KEYX.
041880     IF WS-STATUS = "10"
041890         MOVE "End of FILE" TO WS-STAT-MESSAGE
041900     ELSE
041910     IF WS-STATUS = "22"
041920         MOVE "Duplicate record number" TO WS-STAT-MESSAGE
041930     ELSE
041940     IF WS-STATUS = "23"
041950         MOVE "Invalid record number" TO WS-STAT-MESSAGE
041960     ELSE
041970     IF WS-STATUS = "24"
041980         MOVE "DISK full" TO WS-STAT-MESSAGE
041990     ELSE
042000     IF WS-STATUS = "30"
042010         MOVE "DISK error" TO WS-STAT-MESSAGE
042020     ELSE
042030     IF WS-STATUS = "94"
042040         MOVE "FILE locked" TO WS-STAT-MESSAGE.
045380       DISPLAY "File - " AT 1012 WS-FILE WITH FOREGROUND-COLOR 11.
045390       DISPLAY "Status " AT 1212 
                      WS-STATUS WITH FOREGROUND-COLOR 11
                     ": " WS-STAT-MESSAGE WITH FOREGROUND-COLOR 14.
045400     IF WS-STATUS NOT = "94" 
045410         DISPLAY "Key    " AT 1412 
                        WS-KEY WITH FOREGROUND-COLOR 11
                        " " WS-KEYX WITH FOREGROUND-COLOR 11
045420         DISPLAY "Reverse backup or contact program Support"
                        AT 1612.
045440         DISPLAY "Please make a note of these details" AT 1812.
042170       STOP RUN.
