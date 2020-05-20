      $SET LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *     ******   ********  *******	 *******   ******   ******     *
      *    **	 **	**     **    **  **    **    **    **	 **    *
      *    **		**     **    **  **    **    **    **	       *
      *     ******	**     *******	 *******     **    **	       *
      * 	 **	**     **	 **	     **    **	       *
      *    **	 **	**     **	 **	     **    **	 **    *
      *     ******	**     **	 **	   ******   ******     *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     S T O C K	-   C O R R E C T   P I C   Q U A N T I T Y    *
      *                                                                *
      *     VERSION 8.15.02 - November 2011			       *
      * 							       *
      ******************************************************************
      * 							       *
      * November 2011 - Picking slip quantity not updated when orders  *
      * 		deleted in period end routine. Program amended *
      * 		to add the picking slip quantities to the Stock*
      * 		quantity and clear the picking slip quantity.  *
      * 		After this has been done the program will read *
      * 		the sales order file and adjust picking slip   *
      * 		and stock quantities for any outstanding orders*
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     STPPIC.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   MAY 2003.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 2003 - 2012
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

000120 COPY CONTROL.SL.

000120 COPY PARAM.SL.

000270 COPY SORDER.SL.

000140 COPY STOCK.SL.

000250 DATA DIVISION.
000260 FILE SECTION.

       COPY APACFIDS.FDE.

000360 COPY CONTROL.FDE.

       COPY PARAM.FDE.

000520 COPY SORDER.FDE.

000540 COPY STOCK.FDE.

      /
001690 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
001700 77  WS-DISC	   PIC	999V99	 COMP-3.
001710 77  WS-SUB	   PIC	9(04)	 COMP-5.
001710 77  WS-S1	   PIC	9(04)	 COMP-5.
001720 77  WS-S2	   PIC	9(04)	 COMP-5.
001730 77  WS-S3	   PIC	9(04)	 COMP-5.
001740 77  WS-S4	   PIC	9(04)	 COMP-5.
001750 77  WS-S5	   PIC	9(04)	 COMP-5.
001760 77  WS-S6	   PIC	9(04)	 COMP-5.
001770 77  WS-S7	   PIC	9(04)	 COMP-5.
001780 77  WS-S8	   PIC	9(04)	 COMP-5.
001790 77  WS-IXS	   PIC	9(04)	 COMP-5.
001800 77  WS-IXS1	   PIC	9(04)	 COMP-5.
001810 77  WS-IXS2	   PIC	9(04)	 COMP-5.
001820 77  WS-IXS3	   PIC	9(04)	 COMP-5.
001830 77  WS-IXS4	   PIC	9(04)	 COMP-5.
001860 77  WS-PARKEY	   PIC	9(06)	 COMP-5.
001870 77  WS-NETKEY	   PIC	9(06)	 COMP-5.
002020 77  WS-OPTION	   PIC	X(01).
       77  WS-DELETE	   PIC	X(01).
       77  WS-USE-ITM	   PIC	X(01).
       77  WS-EXT-STK	   PIC	X(01).
002020 77  WS-SKIP	   PIC	X(01).
002040 77  WS-ADJ	   PIC	X(01).
002040 77  WS-PRINT	   PIC	X(01)	 VALUE "N".
002070 77  WS-ERROR	   PIC	9(01).
002220 77  WS-TYPE	   PIC	X(01).
003400 77  WS-PAGE	   PIC	9(04)	 COMP-5 VALUE ZERO.
       77  PRG-PRINTUTL	   PIC	X(12) VALUE "UTP\PRINTUTL".
002280 77  WS-REF	   PIC	X(08).
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
	       05  FILLER      PIC  X(01) VALUE "�".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "�".
	       05  FILLER      PIC  X(01) VALUE "�".
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

002330 COPY WS.WS.

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "STP".
	   03  WS-PROG	       PIC  X(03) VALUE "PIC".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002350 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

       COPY PARAM.ID.

000360 COPY SORDER.ID.

000300 COPY STOCK.ID.

       01  W02-FILE-IDS.
           03  W02-PRINTER.
	       05  W02-REPORT PIC X(07) VALUE "STPPIC.".
               05  W02-USER   PIC X(05) VALUE SPACES.

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

	   03  R-L1 REDEFINES W02-PRN-LINE.
	       05  R-DETAIL.
		   07  R-HL	   PIC	X(80).
		   07  FILLER	   PIC	X(52).

	   03  R-L2 REDEFINES W02-PRN-LINE.
	       05  R-PGE	   PIC X(06).
	       05  R-PNO	   PIC Z(03)9.
	       05  FILLER	   PIC X(09).
	       05  R-CPY.
		   07  FILLER	   PIC	X(05).
		   07  R-HEAD	   PIC	X(37).
	       05  FILLER	   PIC X(03).
	       05  R-DATE	   PIC X(06).
	       05  R-DTE	   PIC Z9/99/9999.

	   03  PSQ-L1 REDEFINES W02-PRN-LINE.
	       05  FILLER	   PIC	X(09).
	       05  PSQ-H1	   PIC	X(19).
	       05  PSQ-H2	   PIC	X(34).
	       05  PSQ-H3	   PIC	X(09).
	       05  FILLER	   PIC	X(09).

	   03  PSQ-L2 REDEFINES W02-PRN-LINE.
	       05  FILLER	   PIC	X(09).
	       05  PSQ-EXT-ITEM.
		   07  PSQ-ITEM	   PIC	X(14).
		   07  PSQ-SL	   PIC	X(01).
		   07  PSQ-ITM	   PIC	X(03).
	       05  FILLER	   PIC	X(01).
	       05  PSQ-DESC	   PIC	X(30).
	       05  FILLER	   PIC	X(01).
	       05  PSQ-QUANT	   PIC	Z(06)9.999-.
	       05  FILLER	   PIC	X(09).

       COPY W05.VAT.

       01  W05-EXTRA.
	   03  W05-ROUND       PIC  X(01).
           03  W05-ROUND-VAL   PIC  9V9(02).
           03  W05-RVAL REDEFINES W05-ROUND-VAL.
               05  W05-RAND    PIC  9(01).
               05  W05-CENTS   PIC  9(02).
           03  W05-RVAL2 REDEFINES W05-ROUND-VAL.
               05  FILLER      PIC  9(01).
               05  W05-RVC1    PIC  9(01).
               05  W05-RVC2    PIC  9(01).
           03  W05-HLF-VAL     PIC  9V99.
           03  W05-TQTR-VAL    PIC  9V99.
           03  W05-CALC-VAL    PIC  9(02)V99.
           03  W05-CALC-RRCC REDEFINES W05-CALC-VAL.
               05  W05-RVR1    PIC  9(01).
               05  W05-RVRCC   PIC  9(01)V99.

       01  W09-STOCK.
002780	   03  W09-MARKUP     PIC S9(03)V9999  COMP-3.
002790	   03  W09-CMARKUP    PIC S9(03)V9999  COMP-3.
002790	   03  W09-WMARKUP    PIC S9(03)V9999  COMP-3.

       COPY W10.STK.

	   03  W10-CONTACT     PIC  X(30).
	   03  W10-SUPP        PIC  X(05).
	   03  W10-SNAME       PIC  X(40).
	   03  W10-STERMS.
	       05  W10-30      PIC  9(01).
	       05  W10-60      PIC  9(01).
	       05  W10-90      PIC  9(01).
	       05  W10-120     PIC  9(01).
	       05  W10-150     PIC  9(01).
	       05  W10-180     PIC  9(01).
	   03  W10-TERMS REDEFINES W10-STERMS
			       PIC  9(06).
	   03  W10-TEL	       PIC  X(14).

       COPY W11.STK.

       COPY W12.WS.

001360 01  W15-INPUT-AREA.
001480     03  W15-NAME.
001490         05  W15-NCHAR PIC  X(01) OCCURS 40.
001500     03  W15-N         REDEFINES W15-NAME.
001510         05  W15-DESC.
001520             07  W15-ENTRY
                             PIC  X(08).
001530             07  FILLER
                             PIC  X(16).
001540         05  FILLER    PIC  X(16).

       01  W16-CALL.
	   03  W16-ITEM      PIC  X(14).
	   03  W16-CRED      PIC  X(05).
	   03  W16-COST      PIC S9(07)V99 COMP-3.
	   03  W16-CANCEL    PIC  X(01).

       COPY W20.WS.

004310 01  W25-CALCS.
005040     03  W25-RESULT      PIC 9(05)V99.
004330     03  W25-RESULT1 REDEFINES W25-RESULT.
004340         05  W25-WHOLE   PIC 9(05).
004350         05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY     PIC 9(04).
               05  W25-SUB     PIC 9(01).
               05  FILLER      PIC 9(02).
003750     03  W25-SCOST      PIC S9(09)V99 COMP-3.
003760     03  W25-PCOST      PIC S9(09)V99 COMP-3.
003760     03  W25-PCST       PIC S9(09)V99 COMP-3.
003760     03  W25-PTOT       PIC S9(09)V99 COMP-3.
003770     03  W25-TSELL      PIC S9(09)V99 COMP-3.
003770     03  W25-TOTAL      PIC S9(09)V99 COMP-3.
003780     03  W25-VALUE      PIC S9(09)V99 COMP-3.
003780     03  W25-ADJUST     PIC S9(09)V99 COMP-3.

       COPY W40.WS.

       01  W42-ATTRIB5.
           03  FILLER          PIC X(2000).

       01  W42-ATTRIB6.
           03  FILLER          PIC X(2000).

       01  W43-SCREEN5.
           03  FILLER          PIC X(2000).

       01  W43-SCREEN6.
           03  FILLER          PIC X(2000).

       COPY FUNCTION.WS.

003800 COPY W50.WS.

       01  W55-HEADING.
	   03  FILLER	      PIC  X(32) VALUE
	      "Picking Slip quantities deleted.".

       COPY W75.STK.

       01  W75-CKEYS.
	   03  W75-CS	     PIC  9(02) COMP-5.
	   03  W75-CS1	     PIC  9(02) COMP-5.
           03  W75-CNO       PIC  X(05) OCCURS 18.

004120 01  W95-COMP            PIC X(40).

005120 01  W100-EDIT.
003970     03  W100-V2.
003980         05  W100-S6V2   PIC Z(05)9.99-.
           03  W100-V3 REDEFINES W100-V2.
               05  W100-PRICE  PIC X(09).
               05  FILLER      PIC X(01).
           03  W100-V5 REDEFINES W100-V2.
	       05  W100-QNT    PIC  Z(03)9-.
	       05  W100-NO     PIC  Z(03)9.
	       05  FILLER      PIC  X(01).

       01  W200-MEMO.
           03  W200-MEM-FLDS. 
               05  W200-MEM1   PIC  X(60).
               05  W200-MEM2   PIC  X(60).
               05  W200-MEM3   PIC  X(60).
               05  W200-MEM4   PIC  X(60).
               05  W200-MEM5   PIC  X(60).
               05  W200-MEM6   PIC  X(60).
               05  W200-MEM7   PIC  X(60).
               05  W200-MEM8   PIC  X(60).
               05  W200-MEM9   PIC  X(60).
               05  W200-MEM10  PIC  X(60).
               05  W200-MEM11  PIC  X(60).
               05  W200-MEM12  PIC  X(60).
           03  W200-MEM-FOC REDEFINES W200-MEM-FLDS.
               05  W200-MEM    PIC  X(60) OCCURS 12.

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
004130 SCREEN SECTION.

       COPY BLANK.CRT.

004850 01  S04C.
004860	   03  LINE  2 COLUMN 26 FOREGROUND-COLOR 7 HIGHLIGHT
004870				 VALUE "CORRECT PICKING SLIP QUANTITY".

004870 COPY S99.CRT.

       COPY ERROR.CRT.

       COPY OPT.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
007740 AA000-MAIN	       SECTION.
007750 AA00.
007760       PERFORM ZA000-INIT.
	     PERFORM BA000.
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE 1		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "C"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
043610	     CLOSE SORDER.

       AA49.
	     EXIT PROGRAM.

       COPY FUNCTION.CRT.

       COPY OPTION.CRT.

       COPY LOCKED.RC2.

       COPY CLEAR.CRT.

       COPY ABORT.PRN.

       COPY PRINTUTL.AP3.

      /
      *    ****    READ FILES ROUTINES
      *
008180 AC000-READ              SECTION.

       COPY CONTROL.RD.

       COPY PARAM.RD.

       COPY SORDER.RD.

       COPY STOCK.RD.

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
010170 AD000             SECTION.

       COPY STOCK.WR.

       COPY SORDER.WR.

       COPY APAC.HLP.

      /
      *  ****  E N Q U I R Y   ( O R D E R S )
      *
021000 BA000		       SECTION 50.
021010 BA00.
021020	     DISPLAY CLR-SCREEN.
021030	     DISPLAY S04C.
	     PERFORM SAVE-SCREEN-2.
	     MOVE 12		 TO SHADE-ROW.
	     MOVE 20		 TO SHADE-COL.
	     MOVE 41		 TO SHADE-WIDTH.
	     MOVE 2		 TO SHADE-LINES.
021040	     DISPLAY "旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴"
		      AT 1118 WITH FOREGROUND-COLOR 3 HIGHLIGHT
				   BACKGROUND-COLOR 3
		     "�" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 3.
	     DISPLAY "� " AT 1218 WITH FOREGROUND-COLOR 3 HIGHLIGHT
				       BACKGROUND-COLOR 3
		     "Correct picking slip quantities "
		      WITH FOREGROUND-COLOR 5 BACKGROUND-COLOR 3
		      "(Y/N)   " WITH FOREGROUND-COLOR 5 HIGHLIGHT
				      BACKGROUND-COLOR 3
		     "�" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 3.
	     DISPLAY "�" AT 1318 WITH FOREGROUND-COLOR 3 HIGHLIGHT
				      BACKGROUND-COLOR 3
		     "컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴켸"
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 3.
	     PERFORM SCREEN-SHADOW.
	     MOVE "N"		 TO WS-OPTION.

       BA05.
021290	     ACCEPT WS-OPTION AT 1258
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 3 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
020250	   IF NOT(WS-OPTION = "Y" OR "N")
	       GO TO BA05.
	     PERFORM RESTORE-SCREEN-2.
	   IF WS-OPTION = "N"
	       GO TO BA200.

       BA08.
010610	     MOVE "!"		 TO STK-CODE.
010620       PERFORM START-AT-STOCK-CODE THRU READ-STOCK-EXIT.
	   IF WS-F-ERROR = 22
	       MOVE "There are no stock records on file"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BA999.
	   IF WS-PRINT = "N"
	       DISPLAY "Clearing picking slip quantities"
			AT 1025 WITH FOREGROUND-COLOR 7 HIGHLIGHT
	   ELSE
	       DISPLAY "Printing picking slip quantities"
			AT 1025 WITH FOREGROUND-COLOR 7 HIGHLIGHT.
	     DISPLAY "Reading Stock Record:" AT 1222.

       BA10.
	     PERFORM READ-STOCK-NEXT-LOCK THRU READ-STOCK-EXIT.
	   IF WS-F-ERROR = 22
	       GO TO BA15.
	     DISPLAY STK-CODE AT 1244
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT.
	   IF NOT(STK-QPIC = ZERO)
	       PERFORM BA100
	       IF WS-PRINT = "N"
		   ADD STK-QPIC  TO STK-QUANT
		   MOVE ZERO	 TO STK-QPIC
	       END-IF
017190	       PERFORM REWRITE-STOCK-UNLOCK THRU WRITE-STOCK-EXIT.
	     GO TO BA10.

       BA15.
	     PERFORM RESTORE-SCREEN-2.
	   IF WS-PRINT = "Y"
	       GO TO BA999.
	     DISPLAY "Recalculating Picking Slip Quantities" AT 1022
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
	     DISPLAY "Updating Stock Record:" AT 1222.
035250	     MOVE SPACES	 TO SOR-REF.
035260       MOVE ZERO           TO SOR-SQN.
035270       PERFORM START-AT-ORDER-NO THRU READ-SORDER-EXIT.
035280     IF WS-F-ERROR = 20
	       MOVE "No order records on file"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BA999.

       BA20.
035310	     PERFORM READ-SORDER-NEXT THRU READ-SORDER-EXIT.
035320     IF WS-F-ERROR = 20
	       MOVE "Picking slip quantities corrected"
				 TO WS-ERR-MES
	       MOVE SPACE	 TO WS-OPTION
	       PERFORM OPT-MESSAGE
	       GO TO BA999.
	   IF NOT(SOR-TYPE = 0)
	       MOVE "Order file corrupted missing header"
				 TO WS-ERR-STRING
	       MOVE SPACE	 TO WS-OPTION
003260	       PERFORM OPT-MESSAGE
	       MOVE SPACES	 TO WS-ERR-MES
033840	       STRING "Ref Number "
		    DELIMITED SIZE
		    SOR-REF DELIMITED SIZE
		    " will be deleted" DELIMITED SIZE
				 INTO WS-ERR-MES
033910	       PERFORM OPT-MESSAGE
	       MOVE SOR-REF	 TO WS-REF
	       MOVE "Y" 	 TO WS-DELETE
	       PERFORM DELETE-SORDER-REC THRU WRITE-SORDER-EXIT
	       GO TO BA35.
	   IF NOT SOR-ORDER
	       MOVE SOR-REF	 TO WS-REF
	       MOVE "N" 	 TO WS-DELETE
	       GO TO BA35.

       BA25.
035310	     PERFORM READ-SORDER-NEXT THRU READ-SORDER-EXIT.
035320     IF WS-F-ERROR = 20
	       MOVE "Order file corrupted missing trailer"
				 TO WS-ERR-STRING
	       MOVE SPACE	 TO WS-OPTION
003260	       PERFORM OPT-MESSAGE
	       MOVE SPACES	 TO WS-ERR-MES
033840	       STRING "Ref Number "
		    DELIMITED SIZE
		    SOR-REF DELIMITED SIZE
				 INTO WS-ERR-MES
033910	       PERFORM OPT-MESSAGE
	       GO TO BA999.
	   IF SOR-TYPE = 1
	       GO TO BA30.
	   IF SOR-TYPE = 9
	       GO TO BA20.
	     GO TO BA25.

       BA30.
	   IF SOR-EXT-ITEM = SPACES
	       GO TO BA25.
	     DISPLAY SOR-EXT-ITEM AT 1245
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT.
	     MOVE SOR-EXT-ITEM	 TO STK-CODE.
	     PERFORM READ-STOCK-LOCK THRU READ-STOCK-EXIT.
	   IF WS-F-ERROR = 22
	       MOVE "Stock file corrupted"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BA999.
	     SUBTRACT SOR-QNT	 FROM STK-QUANT.
	     ADD SOR-QNT	 TO STK-QPIC.
	     PERFORM REWRITE-STOCK-UNLOCK THRU WRITE-STOCK-EXIT.
	     GO TO BA25.

       BA35.
035310	     PERFORM READ-SORDER-NEXT THRU READ-SORDER-EXIT.
035320     IF WS-F-ERROR = 20
	       MOVE "Order file corrupted"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BA999.
	   IF NOT (SOR-REF = WS-REF)
	       PERFORM READ-SORDER-PREV THRU READ-SORDER-EXIT
	       GO TO BA20.
	   IF WS-DELETE = "Y"
	       PERFORM DELETE-SORDER-REC THRU WRITE-SORDER-EXIT.
	   IF NOT (SOR-TYPE = 9)
	       GO TO BA35.
	      GO TO BA20.

       BA100.
	   IF WS-PAGE = ZERO
      *
      *  ***  ****  ***** *   *   ****	****  *** *   * ***** ***** ****
      * *   * *   * *	  **  *   *   * *   *  *  **  *   *   *     *	*
      * *   * ****  ***   * * *   ****	****   *  * * *   *   ***   ****
      * *   * *     *	  *  **   *	*   *  *  *  **   *   *     *	*
      *  ***  *     ***** *   *   *	*   * *** *   *   *   ***** *	*
      *
	       MOVE 8		 TO W02-PRN-TYPE
	       PERFORM OPEN-PRINTER
	       MOVE "P"		 TO WS-COMMAND
	       PERFORM BB000.
012990	     MOVE SPACES	 TO R-DETAIL.
	     MOVE STK-CODE	 TO PSQ-EXT-ITEM.
	     MOVE STK-DESC	 TO PSQ-DESC.
	     MOVE STK-QPIC	 TO PSQ-QUANT.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009800	       PERFORM BB000.
	     PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
				 TO R-DETAIL
	       MOVE 99		 TO WS-ADVANCE
	       MOVE "X" 	 TO WS-OPTION
	       PERFORM CALL-PRINTUTL
014990	       GO TO BA999.
012990	     MOVE SPACES	 TO R-DETAIL.

       BA199.
	     EXIT.

       BA200.
	     MOVE 12		 TO SHADE-ROW.
	     MOVE 20		 TO SHADE-COL.
	     MOVE 41		 TO SHADE-WIDTH.
	     MOVE 2		 TO SHADE-LINES.
021040	     DISPLAY "旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴"
		      AT 1118 WITH FOREGROUND-COLOR 3 HIGHLIGHT
				   BACKGROUND-COLOR 3
		     "�" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 3.
	     DISPLAY "� " AT 1218 WITH FOREGROUND-COLOR 3 HIGHLIGHT
				       BACKGROUND-COLOR 3
		     "Print - picking slip quantities "
		      WITH FOREGROUND-COLOR 5 BACKGROUND-COLOR 3
		      "(Y/N)   " WITH FOREGROUND-COLOR 5 HIGHLIGHT
				      BACKGROUND-COLOR 3
		     "�" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 3.
	     DISPLAY "�" AT 1318 WITH FOREGROUND-COLOR 3 HIGHLIGHT
				      BACKGROUND-COLOR 3
		     "컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴켸"
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 3.
	     PERFORM SCREEN-SHADOW.
	     MOVE "N"		 TO WS-OPTION.

       BA205.
021290	     ACCEPT WS-OPTION AT 1258
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 3 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
020250	   IF NOT (WS-OPTION = "Y" OR "N")
	       GO TO BA205.
	     PERFORM RESTORE-SCREEN-2.
	   IF WS-OPTION = "Y"
	       MOVE WS-OPTION	 TO WS-PRINT
	       MOVE "Picking Slip quantities on file."
			       TO W55-HEADING
	       GO TO BA08.

021720 BA999.
021730       EXIT.

       BB000	       SECTION 51.
       BB00.
      *    *************************************************************
      *    ****   1 O	C P I	( N O R M A L	P R I N T )
      *    *************************************************************
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 1		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL.
	     MOVE "H"		 TO W02-PRN-TYPE.
007080	     MOVE SPACES	 TO R-DETAIL.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
007090	     MOVE "PAGE:"	 TO R-PGE.
007100       MOVE "DATE:"        TO R-DATE.
007110       ADD 1               TO WS-PAGE.
007120       MOVE WS-PAGE        TO R-PNO.
007130       MOVE W12-TODAY      TO R-DTE.
007140       MOVE W95-COMP       TO R-CPY.
	     PERFORM CALL-PRINTUTL.
007160	     MOVE SPACES	 TO R-DETAIL.
012840	     MOVE W55-HEADING	 TO R-HEAD.
	     PERFORM CALL-PRINTUTL.
012860	     MOVE SPACES	 TO R-DETAIL.
	     MOVE "H"		 TO W02-PRN-TYPE.
012910	     MOVE SPACES	 TO R-DETAIL.
	     MOVE ALL "-"	 TO R-HL.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
012910	     MOVE SPACES	 TO R-DETAIL.
012920	     MOVE "Item Code"	 TO PSQ-H1.
012920	     MOVE "Description"	 TO PSQ-H2.
	     MOVE "Quantity"	 TO PSQ-H3.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
012920	     MOVE "_________"	 TO PSQ-H1.
012920	     MOVE "___________"	 TO PSQ-H2.
	     MOVE "________"	 TO PSQ-H3.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
012990	     MOVE SPACES	 TO R-DETAIL.
	     MOVE "D"		 TO W02-PRN-TYPE.

       BB999.
	     EXIT.
      /
044130 ZA000-INIT	       SECTION 90.
044140 ZA000-OPEN.
             PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
044150	     DISPLAY CLR-SCREEN.
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
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
	     MOVE "SORDER"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SORDER.
	     MOVE "STOCKF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STOCKF.
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
044230	     MOVE 3		 TO WS-PARKEY.
044240       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE PAR-USE-ITM	 TO WS-USE-ITM.
	     MOVE PAR-EXT-STK	 TO WS-EXT-STK.
	   IF NOT (PAR-STOCK = "A" OR "N")
	       MOVE "A" 	 TO PAR-STOCK.
	     MOVE PAR-STOCK	 TO W10-STCK.
	   IF PAR-STOCK = "N"
	       MOVE PAR-SLNGTH	 TO W10-SLNGTH
	   ELSE
	       MOVE 14		 TO W10-SLNGTH.
	   IF W10-SLNGTH < 3
	       MOVE 3		 TO W10-SLNGTH
	   ELSE
	   IF W10-SLNGTH > 14
	       MOVE 14		 TO W10-SLNGTH.
	   IF NOT (PAR-SAL-ORD = "P" OR "C")
	       MOVE "P" 	 TO WS-TYPE
	   ELSE
	       MOVE PAR-SAL-ORD  TO WS-TYPE.
044580	     MOVE 1		 TO WS-PARKEY.
044590       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
044600       MOVE PAR-DMY        TO W12-TODAY.
044610       MOVE PAR-YMD        TO W12-T-YMD.
044620       MOVE PAR-COMPANY    TO W95-COMP.
	     GO TO ZA999-EXIT.
      *    *************************************************************
      *    ****   F I L E   L I M I T	E X C E E D E D
      * 	  (Config.sys Dos/Win/Windows 95/98/NT/2000)
      * 	  (Config.nt Windows XP/2000)
      * 	  (System.ini Windows.me)
      *    *************************************************************
039780 ZA49.
039790	     DISPLAY "Too many files OPEN" AT 0708
039800                WITH FOREGROUND-COLOR 14.
039810	     DISPLAY "Check the FILES option in CONFIG.SYS (Dos,Windows
      -		     "3/95/98/NT/2000)" AT 0908.
	     DISPLAY "CONFIG.NT  (Windows XP/2000)" AT 1034.
	     DISPLAY "SYSTEM.INI (Windows ME)" AT 1134.
039820	     DISPLAY "Press any key to continue" AT 1308.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
039840	     GO TO ZA205.

039850 ZA50.
041850	   IF WS-F-ERROR = 20
041860	       MOVE "ORDER file"
				 TO WS-FILE.
039980       DISPLAY "Rebuild " AT 0812
039990                WS-FILE WITH FOREGROUND-COLOR 14.
040000       DISPLAY "Press any key to continue" AT 1012.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
040020 ZA51.
040030       EXIT PROGRAM.

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
	       MOVE "Supervisor�"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.

044790 ZA200.
044800       DISPLAY "Files locked - Try later" AT 2312
044810                WITH FOREGROUND-COLOR 14
                     " " WS-STATUS WITH FOREGROUND-COLOR 15.
044820       DISPLAY "Press any key" AT 2512 
044830		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
044840	     ACCEPT WS-OPTION AT 2526
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
       ZA205.
             EXIT PROGRAM.
044870 ZA999-EXIT.
044880       EXIT.
      /
044890 ZB000-ERROR	       SECTION 91.
      
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
041510       PERFORM DISPLAY-FILE-NAME.
      
041560 DISPLAY-FILE-NAME.
041610     IF WS-F-ERROR = 2
041620         MOVE W02-NETWORK  TO WS-FILE
041630         MOVE WS-NETKEY    TO WS-KEY
041800	   ELSE
041850	   IF WS-F-ERROR = 15
041860         MOVE WS-PARID     TO WS-FILE
041870	       MOVE WS-PARKEY	 TO WS-KEY
041840	   ELSE
041850	   IF WS-F-ERROR = 20
052210         MOVE W02-SORDER   TO WS-FILE
052220         MOVE ZERO         TO WS-KEY
               MOVE SOR-KEY      TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 22
041860         MOVE W02-STOCKF   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE STK-CODE	 TO WS-KEYX.
041880	   IF WS-STATUS = "10"
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
