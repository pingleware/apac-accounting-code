      $SET LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *    **	**    ****    ********	*******   ********  *******    *
      *    **	**   **  **	 **	**    **  **	    **	  **   *
      *    **	**  **	  **	 **	**    **  **	    **	  **   *
      *    **	**  ********	 **	******	  *****     *******    *
      *    **	**  **	  **	 **	**   **   **	    **	       *
      *     ** **   **	  **	 **	**    **  **	    **	       *
      *      ***    **	  **	 **	**    **  ********  **	       *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *   V A L U E   A D D E D   T A X   R E P O R T	P R O G R A M  *
      *                                                                *
      *     VERSION 8.13.08 - August 2008			       *
      * 							       *
      ******************************************************************
      /
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     REP001.
000030 AUTHOR.	       James William Lemmon.
000040 DATE-WRITTEN.   MAY 2005.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 2005 - 2008
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

000060 ENVIRONMENT DIVISION.
000070 CONFIGURATION SECTION.
000080 SPECIAL-NAMES.
000080		       CURSOR IS CSTART
000090		       CONSOLE IS CRT
		       CRT STATUS IS KEY-STATUS.
000100 INPUT-OUTPUT SECTION.
000110 FILE-CONTROL.

       COPY APACFIDS.SL.

000360 COPY CONTROL.SL.

000160 COPY CREDIT.SL.

000140 COPY DEBTOR.ISL.

000420 COPY DEPART.SL.

000440 COPY PARAM.SL.

000350 COPY TXTRAN.SL.

000490 DATA DIVISION.
000500 FILE SECTION.

       COPY APACFIDS.FDE.

000520 COPY CONTROL.FDE.

000540 COPY CREDIT.FDE.

000540 COPY DEBTOR.FDE.

000520 COPY DEPART.FDE.

000520 COPY PARAM.FDE.

000640 COPY TXTRAN.FDE.

      /
      *         **         **    ******    *******    **    **
      *         **         **   **    **   **    **   **   ** 
      *         **    *    **   **    **   **   **    **  **  
      *         **   ***   **   **    **   ******     *****   
      *          ** ** ** **    **    **   **   **    **  **  
      *           ***   ***     **    **   **    **   **   ** 
      *            *     *       ******    **    **   **    **
      *
003220 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
003230 77  WS-CLEAR	   PIC 9(01)	 VALUE ZERO.
       77  WS-AUD	   PIC	9(06)	 COMP-5 VALUE ZERO.
003240 77  WS-S1           PIC 9(04)     COMP-5.
003250 77  WS-S2           PIC 9(04)     COMP-5.
003260 77  WS-S3           PIC 9(04)     COMP-5.
003270 77  WS-S4           PIC 9(04)     COMP-5.
003280 77  WS-S5           PIC 9(04)     COMP-5.
003290 77  WS-S6           PIC 9(04)     COMP-5.
003290 77  WS-S7	   PIC 9(04)	 COMP-5.
003300 77  WS-NO           PIC 9(04)     COMP-5.
003320 77  WS-PARKEY	   PIC 9(04)	 COMP-5.
003330 77  WS-NETKEY	   PIC 9(04)	 COMP-5.
003360 77  WS-AUDKEY	   PIC 9(06)	 COMP-5.
003370 77  WS-INDEX        PIC 9(04)     COMP-5.
003380 77  WS-INDTYPE      PIC 9(01).
003390 77  WS-LINES        PIC 9(02) VALUE 66.
003400 77  WS-PAGE         PIC 9(04)     COMP-5.
       77  PRG-PRINTUTL	   PIC	X(12) VALUE "UTP\PRINTUTL".
002570 77  WS-PGE-LENGTH   PIC	9(02)	 VALUE 66.
001870 77  WS-PRN-LENGTH   PIC	9(02)	 VALUE 62.
003430 77  WS-GST-RATE	   PIC	9(02)V99   COMP-3.
003440 77  WS-OPTION       PIC  X(01).
       77  WS-STATOP       PIC  X(01).
003460 77  WS-REPLY        PIC  X(01).
003470 77  WS-ERROR        PIC  9(01).
003500 77  WS-CHAR1        PIC  X(01).
003510 77  WS-CHAR2        PIC  X(01).
003520 77  WS-COMP         PIC  9(01).
003530 77  WS-TYPE         PIC  X(01).
003550 77  WS-LEDG         PIC  9(03).
003570 77  WS-NUM          PIC  Z9.
003570 77  WS-REF          PIC  X(08).
003590 77  WS-BATCH        PIC  9(05)      COMP-3.
003620 77  WS-REP1	   PIC 9(01) VALUE 0.
003630 77  WS-REP2         PIC 9(01) VALUE 0.
003640 77  WS-REP3         PIC 9(01) VALUE 0.
003650 77  WS-REP4         PIC 9(01) VALUE 0.
003660 77  WS-REP5         PIC 9(01) VALUE 0.
003670 77  WS-REP6         PIC 9(01) VALUE 0.
003680 77  WS-REP7         PIC 9(01) VALUE 0.
003690 77  WS-REP8         PIC 9(01) VALUE 0.
003700 77  WS-REP9         PIC 9(01) VALUE 0.
003710 77  WS-REP10        PIC 9(01) VALUE 0.
003710 77  WS-REP11	   PIC 9(01) VALUE 0.
003720 77  WS-H1	   PIC X(18) VALUE "*** END REPORT ***".
003730 77  WS-H2           PIC X(06) VALUE "M.T.D.".
003740 77  WS-H3           PIC X(06) VALUE "Y.T.D.".
003750 77  WS-H4           PIC X(05) VALUE "TOTAL".
003760 77  WS-H5           PIC X(06) VALUE "CREDIT".
003770 77  WS-H6           PIC X(05) VALUE "DEBIT".
003780 77  WS-H7           PIC X(09) VALUE "ITEM CODE".
003790 77  WS-H8           PIC X(11) VALUE "DESCRIPTION".
003800 77  WS-H9           PIC X(05) VALUE "QUANT".
003810 77  WS-H10          PIC X(04) VALUE "CASH".
003820 77  WS-H11          PIC X(05) VALUE "VALUE".
003830 77  WS-H12          PIC X(03) VALUE "NO.".
003840 77  WS-H13          PIC X(03) VALUE "MTD".
003850 77  WS-H14          PIC X(03) VALUE "YTD".
003860 77  WS-H15          PIC X(08) VALUE "G/LEDGER".
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.

002420 01  WS-DB-LINE.
002430     03  WS-TOP-LNE.
002440         05  WS-TCHR PIC  X(01) OCCURS 80.
           03  WS-T-LINE REDEFINES WS-TOP-LNE.
               05  FILLER  PIC  X(01).
               05  WS-H-LINE
                           PIC  X(78).
               05  FILLER  PIC  X(01).
002430     03  WS-TOP-LNE2.
002440         05  WS-TCH  PIC  X(01) OCCURS 80.
           03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
               05  FILLER      PIC  X(01).
               05  WS-TOP-COMP PIC  X(40).
001430	       05  FILLER      PIC  X(23).
	       05  WS-WRKHD    PIC  X(11).
001430	       05  FILLER      PIC  X(01).
	       05  WS-WRKST    PIC  X(03).
001430	       05  FILLER      PIC  X(01).
002450	   03  WS-MID-LNE.
002460         05  WS-MCHR PIC  X(01) OCCURS 80.
002450     03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "³".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "°".
	       05  FILLER      PIC  X(01) VALUE "³".
002470	   03  WS-BOT-LNE.
002480         05  WS-BCHR PIC  X(01) OCCURS 80.
           03  WS-B-LINE REDEFINES WS-BOT-LNE.
               05  FILLER  PIC  X(01).
               05  WS-F-LINE
                           PIC  X(78).
               05  FILLER  PIC  X(01).
002470     03  WS-BOT-LNE2.
002480         05  WS-BCH  PIC  X(01) OCCURS 80.

003880 COPY WS.WS.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

003900 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

000540 COPY CREDIT.ID.

       COPY DEPART.ID.

       COPY DEBTOR.ID.

       COPY PARAM.ID.

       COPY TXTRAN.ID.

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

000660	   03  R-L1 REDEFINES W02-PRN-LINE.
000670	       05  R-DETAIL	   PIC	X(136).

	   03  VAT-L1 REDEFINES W02-PRN-LINE.
	       05  VAT-DAT	   PIC	99/99/9999.
	       05  FILLER	   PIC	X(01).
	       05  VAT-REF	   PIC	X(08).
	       05  FILLER	   PIC	X(01).
	       05  VAT-TRAN	   PIC	X(11).
	       05  FILLER	   PIC	X(01).
	       05  VAT-NAME.
		   07  FILLER	   PIC	X(04).
		   07  VAT-CDE	   PIC	9(01).
		   07  FILLER	   PIC	X(03).
		   07  VAT-RTE	   PIC	Z9.99.
		   07  VAT-PERC	   PIC	X(01).
	       05  FILLER	   PIC	X(01).
	       05  VAT-TRAN-AMT    PIC	Z(07)9.99-.
	       05  FILLER	   PIC	X(01).
	       05  VAT-NO-TAX	   PIC	Z(06)9.99-.
	       05  FILLER	   PIC	X(01).
	       05  VAT-TXBL-AMT    PIC	Z(07)9.99-.
	       05  FILLER	   PIC	X(01).
	       05  VAT-VAT-AMT	   PIC	Z(06)9.99-.

	   03  VAT-L2 REDEFINES W02-PRN-LINE.
	       05  VAT-H1	   PIC	X(11).
	       05  VAT-H2A.
		   07  VAT-H2	   PIC	X(09).
		   07  VAT-H3	   PIC	X(12).
	       05  VAT-H3A	   PIC	X(15).
	       05  VAT-H4	   PIC	X(13).
	       05  VAT-H5	   PIC	X(16).
	       05  VAT-H6	   PIC	X(09).
	       05  VAT-H7	   PIC	X(11).

000680	   03  VAT-L3 REDEFINES W02-PRN-LINE.
000690	       05  V-PGE	   PIC	X(06).
000700	       05  V-PNO	   PIC	Z(03)9.
	       05  V-HEADING.
000710		   07  FILLER	   PIC	X(18).
000720		   07  V-CPY.
		     09	FILLER	   PIC	X(08).
		     09	V-HEAD	   PIC	X(32).
		   07  V-RATE	   PIC	Z9.99.
		   07  V-PERC	   PIC	X(01).
000730		   07  FILLER	   PIC	X(06).
000740	       05  V-DATE	   PIC	X(06).
000750	       05  V-DTE	   PIC	Z9/99/9999.

       COPY W05.VAT.

       01  W10-DATE.
	   03  W10-DTE	       PIC 99/99/9999.

       COPY W12.WS.

       01  W10-TAX-KEY.
      *
      *    The first record on the file is the control record
      *    and will have the type "C", taxtype 0 and the date
      *    will contain the start date for the period.
      *    
      *    TYPE: "1" = Input Tax
      *          "2" = Output Tax
      *          "3" = Input Payments
      *          "4" = Output Payments
      *
           03  W10-TAX-TYPE    PIC  X(01).
      *
      *    ACTYPE: 1 = Debtors        (Sales / Returns)
      *            2 = Cash           (Sales / Refunds)
      *            3 = Creditors      (Purchases)
      *            4 = General Ledger (Rates, Vehicles ETC)
      *
           03  W10-TAX-ACTYPE  PIC  9(01).
      *
      *    CODE:  This is the transaction code
      *
           03  W10-TAX-CODE    PIC  9(02).
000050	   03  W10-TAX-DATE    PIC  9(08).
           03  W10-TAX-REF     PIC  X(08).
           03  W10-TAX-SEQ     PIC  9(04).
       01  W15-VAT-TOTALS.
	   05  W15-TRAN-AMT-1  PIC S9(09)V99 COMP-3.
	   05  W15-TRAN-AMT-2  PIC S9(09)V99 COMP-3.
	   05  W15-TRAN-AMT-3  PIC S9(09)V99 COMP-3.
	   05  W15-TRAN-AMT-I  PIC S9(09)V99 COMP-3.
	   05  W15-TRAN-AMT-O  PIC S9(09)V99 COMP-3.
	   03  W15-NON-TXBL-1  PIC S9(09)V99 COMP-3.
           03  W15-NON-TXBL-2  PIC S9(09)V99 COMP-3.
           03  W15-NON-TXBL-3  PIC S9(09)V99 COMP-3.
           03  W15-NON-TXBL-I  PIC S9(09)V99 COMP-3.
           03  W15-NON-TXBL-O  PIC S9(09)V99 COMP-3.
	   03  W15-VAT-CODES OCCURS 12.
	   05  W15-TXBL-AMT-1  PIC S9(09)V99 COMP-3.
	   05  W15-TXBL-AMT-2  PIC S9(09)V99 COMP-3.
	   05  W15-TXBL-AMT-3  PIC S9(09)V99 COMP-3.
	   05  W15-TXBL-AMT-I  PIC S9(09)V99 COMP-3.
	   05  W15-TXBL-AMT-O  PIC S9(09)V99 COMP-3.
	   05  W15-VAT-AMT-1   PIC S9(09)V99 COMP-3.
	   05  W15-VAT-AMT-2   PIC S9(09)V99 COMP-3.
	   05  W15-VAT-AMT-3   PIC S9(09)V99 COMP-3.
	   05  W15-VAT-AMT-I   PIC S9(09)V99 COMP-3.
	   05  W15-VAT-AMT-O   PIC S9(09)V99 COMP-3.
       01  W15-HEADINGS.
           03  W15-VAT-TYPE    PIC  X(15).
           03  W15-DESC        PIC  X(15).

       COPY W15.WS.

       COPY W20.WS.

       01  W24-PERIOD.
	   03  FILLER	       PIC X(16) VALUE "For the period: ".
	   03  W24-DTE1        PIC Z9/99/9999.
           03  FILLER          PIC X(04) VALUE " to ".
	   03  W24-DTE2        PIC Z9/99/9999.

004310 01  W25-CALCS.
005040     03  W25-RESULT      PIC 9(05)V99.
004330     03  W25-RESULT1 REDEFINES W25-RESULT.
004340         05  W25-WHOLE   PIC 9(05).
004350         05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY     PIC 9(04).
               05  W25-SUB     PIC 9(01).
               05  FILLER      PIC 9(02).
004360	   03  W25-TOTAL       PIC S9(09)V99  COMP-3.
004370	   03  W25-VALUE       PIC S9(09)V99  COMP-3.
004380	   03  W25-COST1       PIC S9(09)V99  COMP-3.
004390	   03  W25-COST2       PIC S9(09)V99  COMP-3.
004400	   03  W25-COST3       PIC S9(09)V99  COMP-3.
004410	   03  W25-SELL1       PIC S9(09)V99  COMP-3.
004420	   03  W25-SELL2       PIC S9(09)V99  COMP-3.
004430	   03  W25-SELL3       PIC S9(09)V99  COMP-3.
004380	   03  W25-CASH        PIC S9(09)V99  COMP-3.
004390	   03  W25-REFF        PIC S9(09)V99  COMP-3.
004400	   03  W25-INV	       PIC S9(09)V99  COMP-3.
004410	   03  W25-CRN	       PIC S9(09)V99  COMP-3.

       COPY W40.WS.

       COPY FUNCTION.WS.

005120 01  W95-STM.
005130     03  W95-COMP        PIC X(40).

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
      *   ******    ******   *******   ********  ********  **     ** 
      *  **    **  **    **  **    **  **        **        ***    ** 
      *  **        **        **   **   **        **        ** *   ** 
      *   ******   **        ******    *****     *****     **  *  ** 
      *        **  **        **   **   **        **        **   * ** 
      *  **    **  **    **  **    **  **        **        **    *** 
      *   ******    ******   **    **  ********  ********  **     ** 
      *                                                              
005480 SCREEN SECTION.

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

000850 01  S-PROG.
000860	   03  LINE  2 COLUMN 29 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "VALUE ADDED TAX REPORT".

       01  S07.
	   03  LINE 11 COLUMN 33 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC 99/99/9999 USING W15-START AUTO.
	   03  LINE 12 COLUMN 33 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC 99/99/9999 USING W15-END AUTO.
       01  S08.
	   03  LINE  8 COLUMN 67 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC 99/99/9999 USING W15-END AUTO.

004870 COPY S99.CRT.

       COPY ERROR.CRT.

       COPY OPT.CRT.

      /
      *        *******    *******     ******     ****** 
      *        **    **   **    **   **    **   **    **
      *        **    **   **   **    **    **   **
      *        *******    ******     **    **   **
      *        **         **   **    **    **   **
      *        **         **    **   **    **   **    **
      *        **         **    **    ******     ****** 
      *
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
006570 AA00M		  SECTION.
006580 AA00.
003240	   IF LS0-PALEV < 3 AND LS0-SALEV < 9
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA05.
006590	     PERFORM ZA000-INIT.
      *
      *  ***  ****  ***** *   *   ****	****  *** *   * ***** ***** ****
      * *   * *   * *	  **  *   *   * *   *  *  **  *   *   *     *	*
      * *   * ****  ***   * * *   ****	****   *  * * *   *   ***   ****
      * *   * *     *	  *  **   *	*   *  *  *  **   *   *     *	*
      *  ***  *     ***** *   *   *	*   * *** *   *   *   ***** *	*
      *
	     MOVE 8		 TO W02-PRN-TYPE.
	     PERFORM OPEN-PRINTER.
	     MOVE ZERO		 TO W02-LINAGE.
	     MOVE "P"		 TO WS-COMMAND.
	     MOVE "D"		 TO W02-PRN-TYPE.
	     MOVE SPACES	 TO R-L1.

006620 AA01.
	   IF MOUSE-ATTACHED = "Y"
	       MOVE 66		 TO MOUSE-FUNC
	       MOVE 1		 TO MOUSE-PARAM
	       CALL X"AF" USING MOUSE-FUNC
				MOUSE-PARAM.
006630       DISPLAY CLR-SCREEN.
	     DISPLAY S-PROG.
	     PERFORM MA00R.

       AA05.
	     CLOSE CREDIT
		   DEBTOR.

       AA99.
             EXIT PROGRAM.

       COPY DATE.CHK.

       COPY LOCKED.REC.

       COPY FUNCTION.CRT.

       COPY OPTION.CRT.

       COPY CLEAR.CRT.

       COPY ABORT.PRN.

       COPY PRINTUTL.AP6.

      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	       R E A D	 F I L E S   R O U T I N E S	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
007310 AN00-READ	       SECTION.

008650 READ-CONTROL.
008660       READ NETWORK.
008670     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
008680         GO TO READ-CONTROL-EXIT.
008690     IF WS-STAT1 = "2" OR "3" OR "4"
008700         MOVE 2            TO WS-F-ERROR
008710         PERFORM READ-ERROR.
008720     IF RECORD-LOCKED
008730         PERFORM LOCKED-RECORD
008740         GO TO READ-CONTROL.
008750       MOVE 2              TO WS-F-ERROR.
008760       PERFORM READ-ERROR.

008930 READ-CONTROL-EXIT.
008940       EXIT.
      *
      *     ***  ****  ***** ****  *** *****     ***** *** *     *****
      *    *   * *   * *     *   *  *    *       *      *  *     *
      *    *     *   * *     *   *  *    *       *      *  *     *
      *    *     ****  ***   *   *  *    *       ***    *  *     ***
      *    *     *   * *     *   *  *    *       *      *  *     *
      *    *   * *   * *     *   *  *    *       *      *  *     *
      *     ***  *   * ***** ****  ***   *       *     *** ***** *****
      *
006080 READ-CREDIT.
006090       READ CREDIT
               KEY IS P-NUMBER.
006100     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110         GO TO READ-CREDIT-EXIT.
           IF WS-STATUS = "23"
               MOVE 3            TO WS-F-ERROR
               GO TO READ-CREDIT-EXIT.
006120     IF WS-STAT1 = "2" OR "3" OR "4"
006130         MOVE 3            TO WS-F-ERROR
006140         PERFORM READ-ERROR.
006150     IF RECORD-LOCKED
006160         PERFORM LOCKED-RECORD
006170         GO TO READ-CREDIT.

006520 READ-CREDIT-EXIT.
006530       EXIT.
      *
      *    ****  ***** ****    *   ****  *****	   ***** *** *	   *****
      *    *   * *     *   *  * *  *   *   *	   *	  *  *	   *
      *    *   * *     *   * *	 * *   *   *	   *	  *  *	   *
      *    *   * ***   ****  ***** ****    *	   ***	  *  *	   ***
      *    *   * *     *   * *	 * *  *    *	   *	  *  *	   *
      *    *   * *     *   * *	 * *   *   *	   *	  *  *	   *
      *    ****  ***** *   * *	 * *   *   *	   *	 *** ***** *****
      *
008650 READ-DEPART.
008660	     READ DEPART	 WITH IGNORE LOCK
	       KEY IS DPT-CODE.
008670     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
008680         GO TO READ-DEPART-EXIT.
	   IF WS-STATUS = "23"
	       MOVE 7		 TO WS-F-ERROR
	       GO TO READ-DEPART-EXIT.
008690	   IF WS-STAT1 = "2" OR "3" OR "4"
008700         MOVE 7            TO WS-F-ERROR
008710         PERFORM READ-ERROR.

008930 READ-DEPART-EXIT.
008940       EXIT.

      *
      *    ****  ***** ****  *****  ***  ****     ***** *** *     *****
      *    *   * *     *   *   *   *   * *   *    *      *  *     *
      *    *   * *     *   *   *   *   * *   *    *      *  *     *
      *    *   * ***   ****    *   *   * ****     ***    *  *     ***
      *    *   * *     *   *   *   *   * *   *    *      *  *     *
      *    *   * *     *   *   *   *   * *   *    *      *  *     *
      *    ****  ***** ****    *    ***  *   *    *     *** ***** *****
      *
000020 READ-DEBTOR.
000030       READ DEBTOR
               KEY IS DEB-ACNO.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050         GO TO READ-DEBTOR-EXIT.
           IF WS-STATUS = "23"
               MOVE 6            TO WS-F-ERROR
000050         GO TO READ-DEBTOR-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070         MOVE 6            TO WS-F-ERROR
000080         PERFORM READ-ERROR.
000090     IF RECORD-LOCKED
000100         PERFORM LOCKED-RECORD
000110         GO TO READ-DEBTOR.

000470 READ-DEBTOR-EXIT.
000480       EXIT.
      
008690 READ-PARAM.
008700       READ PARAM.
008710     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
008720         GO TO READ-PARAM-EXIT.
008730     IF WS-STAT1 = "2" OR "3" OR "4"
008740         MOVE 15           TO WS-F-ERROR
008750         PERFORM READ-ERROR.
008760     IF RECORD-LOCKED
008770         PERFORM LOCKED-RECORD
008780         GO TO READ-PARAM.
008790       MOVE 15             TO WS-F-ERROR.
008800       PERFORM READ-ERROR.
      
008960 READ-PARAM-EXIT.
008970       EXIT.
      *
      *  ***** *   * ***** ****    *   *   *     **** *** *    ****  
      *    *   *   *   *   *   *  * *  *   *     *     *  *    *     
      *    *    * *    *   *   * *   * **  *     *     *  *    *     
      *    *     *     *   ****  ***** * * *     ***   *  *    ***   
      *    *    * *    *   *   * *   * *  **     *     *  *    *     
      *    *   *   *   *   *   * *   * *   *     *     *  *    *     
      *    *   *   *   *   *   * *   * *   *     *    *** **** ****  
      *
009860 READ-TXTRAN.
009870       READ TXTRAN 
               KEY IS TAX-KEY.                
009880     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
009890         GO TO READ-TXTRAN-EXIT.
           IF WS-STATUS = 23
               MOVE 32           TO WS-F-ERROR
               GO TO READ-TXTRAN-EXIT.
009900     IF WS-STAT1 = "2" OR "3" OR "4"
009910         MOVE 32           TO WS-F-ERROR
009920         PERFORM READ-ERROR.
009930     IF RECORD-LOCKED
009940         PERFORM LOCKED-RECORD
009950         GO TO READ-TXTRAN.
009980       GO TO READ-TXTRAN-EXIT.

008190 READ-TXTRAN-NEXT.
008200       READ TXTRAN NEXT.
008210     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
008220         GO TO READ-TXTRAN-EXIT.
009900     IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
009910         MOVE 32           TO WS-F-ERROR
009920         GO TO READ-TXTRAN-EXIT.
008230     IF WS-STAT1 = "2" OR "3" OR "4"
008240         MOVE 32           TO WS-F-ERROR
008250         PERFORM READ-ERROR.
008260     IF RECORD-LOCKED
008270         PERFORM LOCKED-RECORD
008280         GO TO READ-TXTRAN-NEXT.
008310       GO TO READ-TXTRAN-EXIT.

008190 START-AT-TXTRAN-KEY.
008200       START TXTRAN 
               KEY >= TAX-KEY.
008210     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
008220         GO TO READ-TXTRAN-EXIT.
009900     IF WS-STATUS = "23"
009910         MOVE 32           TO WS-F-ERROR
009920         GO TO READ-TXTRAN-EXIT.
008230     IF WS-STAT1 = "2" OR "3" OR "4"
008240         MOVE 32           TO WS-F-ERROR
008250         PERFORM READ-ERROR.

010150 READ-TXTRAN-EXIT.
010160       EXIT.
      /
      *    ****    V A T   R E P O R T   &   C L E A R
      * 
       MA00R	       SECTION 50.
       MA00.
026010       DISPLAY CLR-SCREEN.
             DISPLAY "DETAILED VAT REPORT" AT 0231
                      WITH FOREGROUND-COLOR 15.

       MA02.
026030	     DISPLAY "Busy printing VAT detailed report" AT 0620.
026050	     DISPLAY "Are you sure Y/N?" AT 0820
026060                WITH FOREGROUND-COLOR 14.
       MA05.
026080	     ACCEPT WS-OPTION AT 0838
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
026140	   IF NOT (WS-OPTION = "Y" OR "N")
               GO TO MA05.
           IF WS-OPTION = "N"
               GO TO MA999.
	     DISPLAY "For the period." AT 1020.
	     DISPLAY "Start date :" AT 1120.
	     DISPLAY "End date   :" AT 1220.
	     DISPLAY "Dates to be keyed in as DDMMYYYY" AT 1420
                      WITH FOREGROUND-COLOR 11.
             MOVE ZERO           TO W15-START W15-END.

       MA06.
             ACCEPT S07.
	     PERFORM CHECK-CORRECT.
018520	   IF WS-OPTION = "N"
018530         GO TO MA06.
	     MOVE W15-START	 TO W10-DTE.
             PERFORM CHECK-DATE.
027400     IF WS-ERROR = 1
027410	       MOVE "Start date" TO WS-ERR-STRING
	       PERFORM ERROR-MESSAGE
               GO TO MA06.
             MOVE W20-DTE        TO W15-S-YMD.
             MOVE W15-END        TO W10-DTE.
             PERFORM CHECK-DATE.
027400     IF WS-ERROR = 1
027410	       MOVE "End date"	 TO WS-ERR-STRING
	       PERFORM ERROR-MESSAGE
               GO TO MA06.
             MOVE W20-DTE        TO W15-E-YMD.
	     MOVE W15-START	 TO W24-DTE1.
	     MOVE W15-END	 TO W24-DTE2.
	     DISPLAY "A" AT 1420 WITH FOREGROUND-COLOR 6 HIGHLIGHT
		     "ll/" "D" WITH FOREGROUND-COLOR 6 HIGHLIGHT
		     "ebtors/" "C" WITH FOREGROUND-COLOR 6 HIGHLIGHT
		     "reditors Transactions :".
	     MOVE "A"		 TO WS-TYPE.

       MA06A.
	     ACCEPT WS-TYPE AT 1457
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-TYPE
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-TYPE = "A" OR "C" OR "D")
	       GO TO MA06A.
	   IF WS-PRINTER = "S"
	       PERFORM SAVE-SCREEN-3.
	     MOVE 1		 TO W05-V-RATE.

       MA08.
	     INITIALIZE W10-TAX-KEY.
	     MOVE W10-TAX-KEY	 TO TAX-KEY.
	     MOVE "1"		 TO TAX-TYPE.
	     MOVE "0"		 TO W10-TAX-TYPE.
	     PERFORM START-AT-TXTRAN-KEY THRU READ-TXTRAN-EXIT.
	   IF WS-F-ERROR = 32
               GO TO MA75.
	   IF NOT (WS-PRINTER = "S")
	       DISPLAY "Reading TAX record:" AT 1620
			WITH FOREGROUND-COLOR 7.
	     MOVE ZERO		 TO W15-TRAN-AMT-1
				    W15-TRAN-AMT-2
				    W15-TRAN-AMT-3
				    W15-TRAN-AMT-I
				    W15-TRAN-AMT-O
				    W15-NON-TXBL-1
                                    W15-NON-TXBL-2
                                    W15-NON-TXBL-3
                                    W15-NON-TXBL-I
				    W15-NON-TXBL-O.
	     MOVE 1		 TO WS-S7.

       MA09.
	     MOVE ZERO		 TO W15-TXBL-AMT-1(WS-S7)
				    W15-TXBL-AMT-2(WS-S7)
				    W15-TXBL-AMT-3(WS-S7)
				    W15-TXBL-AMT-I(WS-S7)
				    W15-TXBL-AMT-O(WS-S7)
				    W15-VAT-AMT-1(WS-S7)
				    W15-VAT-AMT-2(WS-S7)
				    W15-VAT-AMT-3(WS-S7)
				    W15-VAT-AMT-I(WS-S7)
				    W15-VAT-AMT-O(WS-S7).
	   IF WS-S7 < 12
	       ADD 1		 TO WS-S7
	       GO TO MA09.
             MOVE ZERO           TO WS-PAGE.
      *
      *    ****    S E T   P R I N T E R   T O   1 2 C P I
      *
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 2		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
	     GO TO MA15.

       MA10.
	     MOVE W05-VAT-CODE	 TO DPT-CODE.
	     PERFORM READ-DEPART THRU READ-DEPART-EXIT.
007080	     MOVE SPACES	 TO R-DETAIL.
007090       MOVE "PAGE:"        TO V-PGE.
007100       MOVE "DATE:"        TO V-DATE.
007110       ADD 1               TO WS-PAGE.
007120       MOVE WS-PAGE        TO V-PNO.
007130       MOVE W12-TODAY      TO V-DTE.
007140       MOVE W95-COMP       TO V-CPY.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
007160	     MOVE SPACES	 TO R-DETAIL.
             MOVE "* V A T   R E P O R T *" 
                                 TO V-HEAD.
      *	     MOVE DPT-RATE	 TO V-RATE.
      *	     MOVE "%"		 TO V-PERC.
	     PERFORM CALL-PRINTUTL.
	     MOVE W15-VAT-TYPE	 TO R-DETAIL.
	     MOVE W24-PERIOD	 TO V-CPY.
	     PERFORM CALL-PRINTUTL.
007160	     MOVE SPACES	 TO R-DETAIL.
             MOVE "Trn-date"     TO VAT-H1.
             MOVE "Doc.numb"     TO VAT-H2.
             MOVE "Tran-type"    TO VAT-H3.
             MOVE "Account Name" TO VAT-H3A.
             MOVE "Tran-amount"  TO VAT-H4.
             MOVE "Non-taxable"  TO VAT-H5.
             MOVE "Taxable"      TO VAT-H6.
             MOVE "VAT-amount"   TO VAT-H7.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
007160	     MOVE SPACES	 TO R-DETAIL.

       MA15.
	     PERFORM READ-TXTRAN-NEXT THRU READ-TXTRAN-EXIT.
	   IF WS-F-ERROR = 32
               GO TO MA70.
	   IF NOT (WS-PRINTER = "S")
	       DISPLAY TAX-DATE AT 1640 WITH FOREGROUND-COLOR 14
		       " " TAX-REF WITH FOREGROUND-COLOR 15.
           IF (TAX-DATE < W15-S-YMD) OR 
              (TAX-DATE > W15-E-YMD)
               GO TO MA15.
	   IF NOT (WS-TYPE = "A")
	       IF WS-TYPE = "D"
		   IF NOT (TAX-ACTYPE = 1 OR 2)
		       GO TO MA15
		   END-IF
	       ELSE
		   IF NOT (TAX-ACTYPE = 3)
		       GO TO MA15.
	     MOVE 1		 TO WS-S7.
           IF TAX-TYPE NOT = W10-TAX-TYPE
               IF W10-TAX-TYPE = "0"
                   PERFORM MA40
                   PERFORM MA55
                   PERFORM MA10
               ELSE
		   PERFORM MA20 THRU MA24
		   PERFORM MA25 THRU MA28
		   PERFORM MA35 THRU MA38
                   IF TAX-TYPE > 2 AND W10-TAX-TYPE < 3
                       PERFORM MA45 THRU MA50
                   ELSE
                       PERFORM MA40
           ELSE
               IF TAX-ACTYPE NOT = W10-TAX-ACTYPE
		   PERFORM MA20 THRU MA24
		   PERFORM MA25 THRU MA28
		   PERFORM MA30 THRU MA34
               ELSE
                   IF TAX-CODE NOT = W10-TAX-CODE
		       PERFORM MA20 THRU MA24.
	     MOVE 1		 TO WS-S7.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO R-L1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE "P"		 TO WS-COMMAND
	       MOVE SPACES	 TO R-DETAIL
014990	       GO TO MA999.
             MOVE TAX-KEY        TO W10-TAX-KEY.
	     MOVE TAX-DATE	 TO W22-DTE2.
	     MOVE W22-CC2	 TO W22-CC3.
	     MOVE W22-YY2	 TO W22-YY3.
	     MOVE W22-MM2	 TO W22-MM3.
	     MOVE W22-DD2	 TO W22-DD3.
	     MOVE W22-DTE3	 TO VAT-DAT.
             MOVE TAX-REF        TO VAT-REF.
             MOVE W15-DESC       TO VAT-TRAN.
           IF TAX-ACTYPE = 1
               MOVE TAX-AC       TO DEB-ACNO
               PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT
               MOVE DEB-NAME     TO VAT-NAME
               INSPECT VAT-NAME REPLACING ALL "*" BY " "
               INSPECT VAT-NAME REPLACING ALL "!" BY " "
           ELSE
           IF TAX-ACTYPE = 2
               MOVE "Cash Sales" TO VAT-NAME
           ELSE
           IF TAX-ACTYPE = 3
	       MOVE TAX-AC	 TO P-NUMBER
               PERFORM READ-CREDIT THRU READ-CREDIT-EXIT
               MOVE P-NAME       TO VAT-NAME
           ELSE
               MOVE "General Ledger"
                                 TO VAT-NAME.
             MOVE TAX-VALUE      TO VAT-TRAN-AMT.
             MOVE TAX-TAXFREE    TO VAT-NO-TAX.
	     ADD TAX-TAXFREE	 TO W15-NON-TXBL-1
                                    W15-NON-TXBL-2
                                    W15-NON-TXBL-3.
	     ADD TAX-VALUE	 TO W15-TRAN-AMT-1
                                    W15-TRAN-AMT-2
                                    W15-TRAN-AMT-3.
	   IF TAX-TYPE = "1" OR "3"
               ADD TAX-VALUE     TO W15-TRAN-AMT-I
               ADD TAX-TAXFREE   TO W15-NON-TXBL-I
	   ELSE
               ADD TAX-VALUE     TO W15-TRAN-AMT-O
	       ADD TAX-TAXFREE	 TO W15-NON-TXBL-O.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009750	       MOVE SPACES	 TO R-DETAIL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       PERFORM MA10.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO R-L1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE SPACES	 TO R-DETAIL
	       MOVE "P"		 TO WS-COMMAND
014990	       GO TO MA999.

       MA17.
	   IF TAX-VAL(WS-S7) = ZERO
	       GO TO MA18.
	   IF WS-S7 < 7
	       MOVE WS-S7	 TO VAT-CDE
	   ELSE
	       SUBTRACT 6 FROM WS-S7
				 GIVING VAT-CDE.
	     MOVE W05-VAT(WS-S7) TO VAT-RTE.
	     MOVE "%"		 TO VAT-PERC.
	     MOVE TAX-VAT(WS-S7) TO VAT-VAT-AMT.
	     MOVE TAX-VAL(WS-S7) TO VAT-TXBL-AMT.
	     ADD TAX-VAL(WS-S7)  TO W15-TXBL-AMT-1(WS-S7)
				    W15-TXBL-AMT-2(WS-S7)
				    W15-TXBL-AMT-3(WS-S7).
	     ADD TAX-VAT(WS-S7)	 TO W15-VAT-AMT-1(WS-S7)
				    W15-VAT-AMT-2(WS-S7)
				    W15-VAT-AMT-3(WS-S7).
           IF TAX-TYPE = "1" OR "3"
	       ADD TAX-VAL(WS-S7)
				 TO W15-TXBL-AMT-I(WS-S7)
	       ADD TAX-VAT(WS-S7)
				 TO W15-VAT-AMT-I(WS-S7)
           ELSE
	       ADD TAX-VAL(WS-S7)
				 TO W15-TXBL-AMT-O(WS-S7)
	       ADD TAX-VAT(WS-S7)
				 TO W15-VAT-AMT-O(WS-S7).
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009750	       MOVE SPACES	 TO R-DETAIL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       PERFORM MA10.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO R-L1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE SPACES	 TO R-DETAIL
	       MOVE "P"		 TO WS-COMMAND
014990	       GO TO MA999.

       MA18.
	   IF WS-S7 < 12
	       ADD 1		 TO WS-S7
	       GO TO MA17.
             GO TO MA15.
       MA20.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE SPACES	 TO R-DETAIL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       PERFORM MA10.
             MOVE " Total -"     TO VAT-H2.
             MOVE W15-DESC       TO VAT-TRAN.
             MOVE W15-TRAN-AMT-1 TO VAT-TRAN-AMT.
	     MOVE W15-NON-TXBL-1 TO VAT-NO-TAX.
	     MOVE 1		 TO WS-S7.
      *----------------------------------------------------------------*
      * 							       *
      * This routine to be amended when multiple rates are introduced. *
      * 							       *
      *----------------------------------------------------------------*

       MA22.
	   IF W15-TXBL-AMT-1(WS-S7) = ZERO
	       GO TO MA24.
	     MOVE W15-TXBL-AMT-1(WS-S7)
				 TO VAT-TXBL-AMT.
	     MOVE W15-VAT-AMT-1(WS-S7)
				 TO VAT-VAT-AMT.
	   IF W02-LINAGE < (W02-PRN-LENGTH - 1)
	       MOVE 2		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE SPACES	 TO R-DETAIL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       PERFORM MA10.

	MA24.
	   IF WS-S7 < 7
	       ADD 1		 TO WS-S7
	       GO TO MA22.
             MOVE ZERO           TO W15-TRAN-AMT-1
                                    W15-NON-TXBL-1
				    W15-TXBL-AMT-1(1)
				    W15-TXBL-AMT-1(7)
				    W15-VAT-AMT-1(1)
				    W15-VAT-AMT-1(7).
             PERFORM MA55.

       MA25.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE SPACES	 TO R-DETAIL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       PERFORM MA10.
             MOVE "Total for"    TO VAT-H1.
           IF W10-TAX-ACTYPE = 1
	       MOVE " Debtors"	 TO VAT-H2
           ELSE
               IF W10-TAX-ACTYPE = 2
		   MOVE " Cash"  TO VAT-H2
               ELSE
                   IF W10-TAX-ACTYPE = 3
                       MOVE " Creditors"
                                 TO VAT-H2
                   ELSE
                       MOVE " General ledger" 
                                 TO VAT-H2.
	     MOVE W15-TRAN-AMT-2 TO VAT-TRAN-AMT.
	     MOVE W15-NON-TXBL-2 TO VAT-NO-TAX.
	     MOVE 1		 TO WS-S7.
      *----------------------------------------------------------------*
      * 							       *
      * This routine to be amended when multiple rates are introduced. *
      * 							       *
      *----------------------------------------------------------------*
       MA26.
	   IF W15-TXBL-AMT-2(WS-S7) = ZERO
	       GO TO MA28.
	     MOVE W15-TXBL-AMT-2(WS-S7)
				 TO VAT-TXBL-AMT.
	     MOVE W15-VAT-AMT-2(WS-S7)
				 TO VAT-VAT-AMT.
	   IF W02-LINAGE < (W02-PRN-LENGTH - 1)
	       MOVE 2		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE SPACES	 TO R-DETAIL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       PERFORM MA10.

	MA28.
	   IF WS-S7 < 7
	       ADD 1		 TO WS-S7
	       GO TO MA26.
	     MOVE ZERO		 TO W15-TRAN-AMT-2
                                    W15-NON-TXBL-2
				    W15-TXBL-AMT-2(1)
				    W15-TXBL-AMT-2(7)
				    W15-VAT-AMT-2(1)
				    W15-VAT-AMT-2(2).
       MA30.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE SPACES	 TO R-DETAIL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       PERFORM MA10.
             MOVE "Running Total -"
                                 TO VAT-L1.
             MOVE W15-TRAN-AMT-3 TO VAT-TRAN-AMT.
	     MOVE W15-NON-TXBL-3 TO VAT-NO-TAX.
	     MOVE 1		 TO WS-S7.
      *----------------------------------------------------------------*
      * 							       *
      * This routine to be amended when multiple rates are introduced. *
      * 							       *
      *----------------------------------------------------------------*
       MA32.
	   IF W15-TXBL-AMT-3(WS-S7) = ZERO
	       GO TO MA34.
	     MOVE W15-TXBL-AMT-3(WS-S7)
				 TO VAT-TXBL-AMT.
	     MOVE W15-VAT-AMT-3(WS-S7)
				 TO VAT-VAT-AMT.
	   IF W02-LINAGE < (W02-PRN-LENGTH - 1)
	       MOVE 2		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE SPACES	 TO R-DETAIL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       PERFORM MA10.

       MA34.
	   IF WS-S7 < 7
	       ADD 1		 TO WS-S7
	       GO TO MA32.

       MA35.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE SPACES	 TO R-DETAIL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       PERFORM MA10.
             MOVE "Totals - "    TO VAT-H1.
             MOVE W15-VAT-TYPE   TO VAT-H2A.
             MOVE W15-TRAN-AMT-3 TO VAT-TRAN-AMT.
             MOVE W15-NON-TXBL-3 TO VAT-NO-TAX.
	     MOVE 1		 TO WS-S7.
      *----------------------------------------------------------------*
      * 							       *
      * This routine to be amended when multiple rates are introduced. *
      * 							       *
      *----------------------------------------------------------------*
       MA36.
	   IF W15-TXBL-AMT-3(WS-S7) = ZERO
	       GO TO MA38.
	     MOVE W15-TXBL-AMT-3(WS-S7)
				 TO VAT-TXBL-AMT.
	     MOVE W15-VAT-AMT-3(WS-S7)
				 TO VAT-VAT-AMT.
	   IF W02-LINAGE < (W02-PRN-LENGTH - 1)
	       MOVE 2		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE SPACES	 TO R-DETAIL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       PERFORM MA10.

       MA38.
	   IF WS-S7 < 7
	       ADD 1		 TO WS-S7
	       GO TO MA36.
	     MOVE ZERO		 TO W15-TRAN-AMT-3
                                    W15-NON-TXBL-3
				    W15-TXBL-AMT-3(1)
				    W15-TXBL-AMT-3(7)
				    W15-VAT-AMT-3(1)
				    W15-VAT-AMT-3(7).
       MA40.
           IF TAX-TYPE = 1
               MOVE "Input VAT"  TO W15-VAT-TYPE
           ELSE
           IF TAX-TYPE = 2
               MOVE "Output VAT" TO W15-VAT-TYPE
           ELSE
           IF TAX-TYPE = 3
               MOVE "Input payments"
                                 TO W15-VAT-TYPE
           ELSE
           IF TAX-TYPE = 4
               MOVE "Output payments"
                                 TO W15-VAT-TYPE.
       MA45.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE SPACES	 TO R-DETAIL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       PERFORM MA10.
             MOVE "Net VAT amount"
				 TO VAT-L1.
      *----------------------------------------------------------------*
      * 							       *
      * This routine to be amended when multiple rates are introduced. *
      * 							       *
      *----------------------------------------------------------------*
	     ADD W15-VAT-AMT-I(1) W15-VAT-AMT-O(1)
		 W15-VAT-AMT-I(7) W15-VAT-AMT-O(7)
                                 GIVING VAT-VAT-AMT.
       MA50.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     MOVE "P"		 TO WS-COMMAND.
	     PERFORM MA40.
             PERFORM MA10.
             MOVE ZERO           TO W15-TRAN-AMT-I
                                    W15-NON-TXBL-I
				    W15-TXBL-AMT-I(1)
				    W15-TXBL-AMT-I(7)
				    W15-VAT-AMT-I(1)
				    W15-VAT-AMT-I(7).
             MOVE ZERO           TO W15-TRAN-AMT-O
                                    W15-NON-TXBL-O
				    W15-TXBL-AMT-O(1)
				    W15-TXBL-AMT-O(7)
				    W15-VAT-AMT-O(1)
				    W15-VAT-AMT-O(1).

       MA55.
           IF TAX-ACTYPE = 1
               PERFORM MA60
           ELSE
           IF TAX-ACTYPE = 2
               IF TAX-CODE = 01
                   MOVE "Cash sale"
                                 TO W15-DESC
               ELSE
                   MOVE "Cash refund"
                                 TO W15-DESC
           ELSE
           IF TAX-ACTYPE = 3
               PERFORM MA65
           ELSE
           IF TAX-ACTYPE = 4
               IF TAX-CODE = 01
		   MOVE "Inv/B.Chrg"
				 TO W15-DESC
               ELSE
		   MOVE "D.N./Reversal"
				 TO W15-DESC.
       MA60.
             ADD 21 TAX-CODE     GIVING W25-RESULT.
             DIVIDE W25-RESULT BY 2
                                 GIVING WS-PARKEY
                                 REMAINDER WS-S1.
             ADD 1               TO WS-S1.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE PAR-E-DESC(WS-S1)
                                 TO W15-DESC.
       MA65.
             ADD 381 TAX-CODE    GIVING W25-RESULT.
             DIVIDE W25-RESULT BY 2
                                 GIVING WS-PARKEY
                                 REMAINDER WS-S1.
             ADD 1               TO WS-S1.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE PAR-C-DESC(WS-S1)
                                 TO W15-DESC.
       MA70.
	     PERFORM MA20 THRU MA24.
	     PERFORM MA25 THRU MA28.
	     PERFORM MA35 THRU MA38.
             PERFORM MA45.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO R-DETAIL.
             MOVE "**** E N D   R E P O R T ****"
                                 TO V-CPY.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     MOVE "P"		 TO WS-COMMAND.
      *
      *    ****    RESET THE PRINTER CONTROLS
      *
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 1		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
	     MOVE SPACES	 TO R-DETAIL.

       MA75.
	   IF WS-PRINTER = "S"
	       PERFORM RESTORE-SCREEN-3.
	     MOVE 11		 TO SLIN.
	     MOVE "VAT Report completed - press any key"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.
	     PERFORM OPT-SETUP THRU OPT-EXIT.

       MA999.
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
      *    ****    P A R A M E T E R   F I L E
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
038300	     MOVE 3		 TO WS-PARKEY.
038310	     PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      *
      *    ****    G E T   F I L E   P A T H S	 A N D	 I D ' S
      *
      *    *************************************************************
      *    *** C R E D I T O R	 A C C O U N T	 ( A / P )   F I L E ***
      *    *************************************************************
	     MOVE "CREDIT"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CREDIT.
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
      *    **** 	    C O N T R O L   F I L E		    ****
      *    *************************************************************
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
      *    *************************************************************
      *    ****      V A T   T R A N S A C T I O N S   F I L E	    ****
      *    *************************************************************
	     MOVE "TXTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-TXTRAN.
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
                   GO TO ZA50.
042240     IF WS-STATUS NOT = "00"
042250	       MOVE 6		 TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
050210		   PERFORM OPEN-ERROR.
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

029620 ZA10.
029890	     MOVE 1		 TO WS-PARKEY.
029900       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
029910	     MOVE PAR-DMY	 TO W12-TODAY.
029920	     MOVE PAR-YMD	 TO W12-T-YMD.
029930	     MOVE PAR-COMPANY	 TO W95-COMP.
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
055350	   IF WS-F-ERROR = 6
055360         MOVE "DEBTOR file"
055370				 TO WS-FILE.
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
041610	   IF WS-F-ERROR = 2
041620	       MOVE W02-NETWORK  TO WS-FILE
041630	       MOVE WS-NETKEY	 TO WS-KEY
041600     ELSE
041610     IF WS-F-ERROR = 3
041620	       MOVE W02-CREDIT	 TO WS-FILE
041630	       MOVE ZERO	 TO WS-KEY
041760     ELSE
041770     IF WS-F-ERROR = 6
041780	       MOVE W02-DEBTOR	 TO WS-FILE
041790	       MOVE ZERO	 TO WS-KEY
               MOVE DEB-ACNO     TO WS-KEYX
041800     ELSE
041810     IF WS-F-ERROR = 7
041820	       MOVE W02-DEPART	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
041830	       MOVE DPT-CODE	 TO WS-KEYX
041840	   ELSE
041120	   IF WS-F-ERROR = 15
041130	       MOVE WS-PARID	 TO WS-FILE
041140	       MOVE WS-PARKEY	 TO WS-KEY
041840	   ELSE
041850     IF WS-F-ERROR = 32
041860	       MOVE W02-TXTRAN	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY.
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
