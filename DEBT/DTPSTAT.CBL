      $SET LINKCOUNT"640"
      ******************************************************************
      *                                                                *
      * *******  ******** *******   ******  ********  *****   ******** *
      * **    **    **	  **	** **	 **    **    **   **	 **    *
      * **    **    **	  **	** **	       **    **   **	 **    *
      * **    **    **	  *******   ******     **    *******	 **    *
      * **    **    **	  **		 **    **    **   **	 **    *
      * **    **    **	  **	   **	 **    **    **   **	 **    *
      * *******     **	  **	    ******     **    **   **	 **    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *    D E B T O R ' S   C H A N G E   D A T E S   P R O G R A M   *
      *                                                                *
      *     VERSION 8.15.00 - December 2010			       *
      * 							       *
      ******************************************************************
      * 							       *
      *  May 2006	- New field included in Accounts Receivable    *
      * 		  for recording number of outstanding Jobs     *
      * 		  linked to the account. Additional filler     *
      * 		  added for future expansion.		       *
      * 							       *
      ******************************************************************
      *  ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *  ³ This is a utility program that may be amended from time to  ³
      *  ³ time and is used to make minor or selective corrections     ³
      *  ³ in the Accounts Receivable files.			       ³
      *  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     DTPSTAT.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   April 2005.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 2005 - 2011
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

       COPY CARDEX.SL.

000250 COPY INVOIC.SL.

000270 COPY PARAM.SL.

000200	   SELECT PRNREP  ASSIGN W02-PRINTER
                          ORGANIZATION LINE SEQUENTIAL.

      $SET KEYCOMPRESS "7"
000210	   SELECT SALEXT ASSIGN DISK
                         LOCK AUTOMATIC
000220                   STATUS WS-STATUS
000230                   ACCESS DYNAMIC
000240                   ORGANIZATION INDEXED
000250			 RECORD EXT-KEY.
      $SET NOKEYCOMPRESS

000180 COPY STOCK.SL.

       COPY TXTRAN.SL.

000260 DATA DIVISION.
000270 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CARDEX.FDE.

000540 COPY INVOIC.FDE.

000560 COPY PARAM.FDE.

      /
000330 FD  PRNREP    LABEL RECORD OMITTED
000420		     LINAGE IS WS-PGE-LENGTH.

000350 01  REP-LINE1.
000360	   03  REP-DETAIL1     PIC  X(136).

000390 01  REP-LINE2.
000400	   03  REP-PAGE    PIC	X(06).
000410	   03  REP-P-NO    PIC	Z(03)9.
000420	   03  FILLER	   PIC	X(10).
000430	   03  REP-COMPANY PIC	X(40).
000440	   03  FILLER	   PIC	X(04).
000450	   03  REP-DATE    PIC	X(06).
000460	   03  REP-DTE	   PIC	Z9/99/9999.

       01  REP-LINE3.
	   03  ST-H1	   PIC	X(20).
	   03  ST-H2	   PIC	X(60).

       01  REP-LINE4.
	   03  FILLER	   PIC	X(20).
	   03  ST-H3	   PIC	X(08).
	   03  ST-H4	   PIC	X(11).
	   03  ST-H5	   PIC	X(12).
	   03  ST-H6	   PIC	X(14).
	   03  ST-H7	   PIC	X(15).

       01  REP-LINE4.
	   03  ST-CODE	   PIC	X(18).
	   03  FILLER	   PIC	X(02).
	   03  ST-DESC	   PIC	X(30).
	   03  FILLER	   PIC	X(30).

       01  REP-LINE5.
	   03  FILLER	   PIC	X(20).
	   03  ST-AC	   PIC	X(06).
	   03  FILLER	   PIC	X(02).
	   03  ST-REFER    PIC	X(08).
	   03  FILLER	   PIC	X(02).
	   03  ST-QUANT    PIC	ZZZZZZ9.99.
	   03  FILLER	   PIC	X(02).
	   03  ST-COST	   PIC	ZZZZZZZZ9.99.
	   03  FILLER	   PIC	X(02).
	   03  ST-TOTAL    PIC	ZZZZZZZZ9.99.
	   03  FILLER	   PIC	X(04).

       COPY SALEXT.FDE.

000390 COPY STOCK.FDE.

       COPY TXTRAN.FDE.

      /
      *         **         **    ******    *******    **    **
      *         **         **   **    **   **    **   **   ** 
      *         **    *    **   **    **   **   **    **  **  
      *         **   ***   **   **    **   ******     *****   
      *          ** ** ** **    **    **   **   **    **  **  
      *           ***   ***     **    **   **    **   **   ** 
      *            *     *       ******    **    **   **    **
      *
001550 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
001560 77  WS-SUB	   PIC	9(04)	 COMP-5.
001560 77  WS-S            PIC  9(04)    COMP-5.
001570 77  WS-S1           PIC  9(04)    COMP-5.
001580 77  WS-S2           PIC  9(04)    COMP-5.
001590 77  WS-S3           PIC  9(04)    COMP-5.
001600 77  WS-S4           PIC  9(04)    COMP-5.
001610 77  WS-S5           PIC  9(04)    COMP-5.
001620 77  WS-S6           PIC  9(04)    COMP-5.
001630 77  WS-S7           PIC  9(04)    COMP-5.
001640 77  WS-S8           PIC  9(04)    COMP-5.
001650 77  WS-S9           PIC  9(04)    COMP-5.
002850 77  WS-IXS1         PIC  9(04)    COMP-5.
001690 77  WS-ENQPOS	   PIC	9(06)	 COMP-5.
001700 77  WS-ENQEND       PIC  9(06)    COMP-5.
001670 77  WS-NETKEY	   PIC	9(06)	 COMP-5.
001660 77  WS-PARKEY       PIC  9(06)    COMP-5.
001810 77  WS-KEYSTR       PIC  9(06)    COMP-5.
002780 77  WS-LOOP	   PIC	X(01)	 VALUE "C".
001820 77  WS-OPTION       PIC  X(01).
       77  WS-EXT-STK	   PIC	X(18)	 VALUE SPACES.
001820 77  WS-ENQ	   PIC	X(01).
       77  WS-MONTH	   PIC	9(02).
       77  WS-TCODE	   PIC	9(02).
001820 77  WS-ETYPE	   PIC	X(01).
001850 77  WS-ERROR	   PIC	9(01)	 VALUE ZERO.
       77  WS-USUB	   PIC	9(04)	 COMP-5.
003270 77  WS-PAGE	   PIC	9(04)	 COMP-5.
003150 77  WS-PRN-NO	   PIC	X(01)	 COMP-X VALUE 9.
003160 77  WS-PRN-STAT	   PIC	X(01)	 COMP-X.
002570 77  WS-PGE-LENGTH   PIC	9(02)	 VALUE 66.
001870 77  WS-PRN-LENGTH   PIC	9(02)	 VALUE 62.

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
003290	   03  WS-MID-LNE.
003300	       05  WS-MCHR  PIC  X(80) VALUE "³                ³
      -        "                      ³          ³         ³         ³".
	   03  WS-MST-LNE REDEFINES WS-MID-LNE.
	       05  WS-MST1     PIC  X(01).
	       05  WS-MST78    PIC  X(78).
	       05  WS-MST80    PIC  X(01).
003350	   03  WS-MD-LNE.
003360	       05  WS-MCH   PIC  X(80) VALUE "³      ³
      -        "                      ³           ³          ³       ³".
	   03  WS-MDDT-LNE REDEFINES WS-MD-LNE.
	       05  WS-MDT1     PIC  X(01).
	       05  WS-MDT78    PIC  X(78).
	       05  WS-MDT80    PIC  X(01).
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

002230 COPY WS.WS.

002330 COPY CHAIN.WS.

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "DTP".
	   03  WS-PROG	       PIC  X(04) VALUE "STAT".

002310 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CARDEX.ID.

000140 COPY INVOIC.ID.

       COPY PARAM.ID.

	   03  W02-PRINTER.
	       05  W02-PRN     PIC  X(06).

000970	   03  W02-SALEXT.
000020	       05  W02-PTH43B  PIC  X(03).
000030	       05  FILLER      PIC  X(17) VALUE "_DATA\SALEXT.DAT".

000390 COPY STOCK.ID.

       COPY TXTRAN.ID.

       01  WS-APAC.
	   03  APAC-SYSTEM  PIC  X(12).
	   03  APAC-VERSION PIC  X(04).

002500 01  W10-SPARES.
002510	   03  W10-DATE	    PIC X(10).
002520	   03  W10-EDTE REDEFINES W10-DATE PIC 99/99/9999.

       01  W10-REF.
           03  W10-NUM         PIC  9(08).

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

       COPY W12.WS.

       COPY W15.WS.

       COPY W20.WS.

002850 01  W25-CALCS.
002860     03  W25-RESULT      PIC 9(05)V99.
002870     03  W25-RESULT1 REDEFINES W25-RESULT.
002880         05  W25-WHOLE   PIC 9(05).
002890         05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY    PIC 9(04).
               05  W25-SUB    PIC 9(01).
               05  FILLER     PIC 9(02).
002900     03  W25-TOTAL       PIC S9(09)V99 COMP-3.
002910     03  W25-VALUE       PIC S9(09)V99 COMP-3.
001660	   03  W25-TIME        PIC  9(08).
001670     03  W25-TRED REDEFINES  W25-TIME.
001680	       05  FILLER      PIC  9(02).
001690	       05  W25-FACT    PIC  9(06).
	       05  W25-DATE REDEFINES W25-FACT.
		   07  W25-YY  PIC  9(02).
		   07  W25-MM  PIC  9(02).
		   07  W25-DD  PIC  9(02).
	   03  W25-CUR-CC      PIC  9(02) COMP-5.
	   03  W25-PRV-CC      PIC  9(02) COMP-5.
	   03  W25-NXT-CC      PIC  9(02) COMP-5.


       COPY W40.WS.

       COPY FUNCTION.WS.

003530 COPY W50.WS.

004390 01  W95-STM.
004400     03  W95-COMP        PIC X(40).

      /
004580 SCREEN SECTION.

       COPY BLANK.CRT.

009340 01  S05D.
	   03  LINE  4 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6
			      VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
	   03  LINE  5 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6
			      VALUE "³   Key in Reference Number.   ³".
	   03  LINE  6 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6
			      VALUE "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".
	   03  LINE  7 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6
			      VALUE "³ Invoice Reference :          ³".
	   03  LINE  8 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6
			      VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S05A.
	   03  LINE  7 COLUMN 47 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZZZZZZ9 USING W10-NUM AUTO.

004870 COPY S99.CRT.

       COPY ERROR.CRT.

       COPY OPT.CRT.

      /
007730 PROCEDURE DIVISION.
010250 AA000-MAIN	       SECTION.
010260 AA000-INIT.
010270       PERFORM ZA000-INIT.
	     PERFORM BA000.
	     CLOSE CARDEX
		   INVOICE
		   STOCK
		   TXTRAN.

       AA49.
013150	     STOP RUN.

       COPY FUNCTION.CRT.

       COPY OPTION.CRT.

011390 COPY LOCKED.RC2.

011410 COPY CLEAR.CRT.

       COPY DATE.CHK.

      /
      *    ****    READ FILES ROUTINES
      *
011650 AC000              SECTION.

       COPY CARDEX.RD.

       COPY INVOIC.RD.

011270 COPY PARAM.RD.

       COPY STOCK.RD.

011270 COPY TXTRAN.RD.

      /
      *    ****    WRITE FILES ROUTINES
      *
011650 AD000		  SECTION.

       COPY CARDEX.WR.

       COPY INVOIC.WR.

011270 COPY PARAM.WR.

011270 COPY TXTRAN.WR.

       COPY APAC.HLP.

      /
      *  **** A M E N D	  V A T	  A N D	  S A L E S   P E R S O N
      *       F I G U R E S .	P R I N T   S T A T I S T I C S
      *       O F   C O S T   A N D   S E L L I N G
      *
021000 BA000		       SECTION 50.
021010 BA00.
021020       DISPLAY CLR-SCREEN.
	     DISPLAY "STATISTICS EXTRACTED FOR" AT 0224
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.

       BA05.
	     PERFORM SAVE-SCREEN-3.
	     MOVE 5		 TO SHADE-ROW.
	     MOVE 27		 TO SHADE-COL.
	     MOVE 30		 TO SHADE-WIDTH.
	     MOVE 4		 TO SHADE-LINES.
	     DISPLAY S05D.
	     PERFORM SCREEN-SHADOW.
	     MOVE ZERO		 TO W10-NUM.

       BA10.
	     DISPLAY "Press " AT 2552
		      WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3
		     "Esc" WITH FOREGROUND-COLOR 6 HIGHLIGHT
				BACKGROUND-COLOR 3
		     "ape to Exit"
		      WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3.
	     ACCEPT S05A.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BA30
		 WHEN OTHER	 CALL X"E5"
		     GO TO BA10
	       END-EVALUATE.
	     PERFORM CHECK-CORRECT.
020250	   IF WS-OPTION = "N"
	       GO TO BA10.
	     DISPLAY CLEAR-L25.
	   IF W10-NUM = ZERO
	       GO TO BA30.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Read the Invoice file using the Reference number that has ³
      *    ³ been keyed in by the User. 			       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE W10-NUM	 TO DOC-REF.
	     MOVE ZERO		 TO DOC-SQN DOC-TYPE.
	     PERFORM START-AT-INVOIC-REF THRU READ-INVOIC-EXIT.
	   IF WS-F-ERROR = 12
	       GO TO BA20.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		   Read the next Invoice record 	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       BA15.
045330	     PERFORM READ-INVOIC-NEXT THRU READ-INVOIC-EXIT.
      *
      *    ****   E N D   O F	F I L E   ?
      *
	   IF WS-F-ERROR = 12
045270	       GO TO BA25.
      *
      *    ****   Finished with the Reference number that was keyed in?
      *
045340	   IF (WS-F-ERROR NOT = ZERO) OR
045350	      (DOC-REF NOT = W10-NUM)
045270	       GO TO BA25.
      *
      *    **	I S  T H I S  A N  I T E M  D E T A I L	 R E C O R D ?
      *
045370	   IF DOC-TYPE NOT = 1
045380	       GO TO BA15.
      *
      *    ****   I T E M   D E T A I L S
      *
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Read the Cardex file (Sales) using the Stock code and the ³
      *    ³ Reference number.					       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE DOC-EXT-ITEM	 TO CRD-EXT-CODE.
	     MOVE DOC-REF	 TO CRD-REF.
	     PERFORM START-AT-CRDX-MOVE THRU READ-CARDEX-EXIT.
	   IF WS-F-ERROR = 43
	       MOVE SPACES	 TO WS-ERR-MES
	       STRING "Missing Cardex: " DELIMITED SIZE
		     DOC-EXT-ITEM DELIMITED SIZE
		     " " DELIMITED SIZE
		     DOC-REF DELIMITED SIZE
		     INTO WS-ERR-MES
	       PERFORM ERROR-LENGTH THRU ERROR-EXIT
	       GO TO BA15.
	     PERFORM READ-CARDEX-NEXT THRU READ-CARDEX-EXIT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	  Log the details to the statistics extract file.      ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE CRD-AC	 TO EXT-AC.
	     MOVE CRD-EXT-CODE	 TO EXT-STK-CODE.
	     MOVE CRD-DTE	 TO EXT-DTE.
	     MOVE CRD-SEQ	 TO EXT-SEQ.
	     MOVE CRD-REF	 TO EXT-REF.
	     MOVE CRD-SMAN	 TO EXT-SMAN.
	     MOVE CRD-QNT	 TO EXT-QNT.
	     MOVE CRD-COST	 TO EXT-COST.
	     MOVE CRD-SELL	 TO EXT-SELL.
	     MOVE CRD-NAME	 TO EXT-NAME.
	     MOVE CRD-SUPP	 TO EXT-SUPP.
	     MOVE CRD-WHS	 TO EXT-WHS.
	     MOVE CRD-TIME	 TO EXT-TIME.
	     WRITE EXT-REC1.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	 Move all sales for these Items to Sales person 01     ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
029280	   IF NOT(CRD-SMAN = ZERO)
	       IF NOT(CRD-SMAN = 01)
029290		   ADD 100 CRD-SMAN
				 GIVING WS-PARKEY
		   PERFORM READ-PARAM THRU READ-PARAM-EXIT
		   MULTIPLY CRD-QNT BY CRD-COST
				 GIVING W25-VALUE
		   MULTIPLY CRD-QNT BY CRD-SELL
				 GIVING W25-TOTAL
		   SUBTRACT W25-VALUE
				 FROM PAR-CT-MTD PAR-CT-YTD
		   SUBTRACT W25-TOTAL
				 FROM PAR-SL-MTD PAR-SL-YTD
		   PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT
		   MOVE 101	 TO WS-PARKEY
		   PERFORM READ-PARAM THRU READ-PARAM-EXIT
029310		   ADD W25-VALUE TO PAR-CT-MTD PAR-CT-YTD
029320		   ADD W25-TOTAL TO PAR-SL-MTD PAR-SL-YTD
029330		   PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT
		   PERFORM DELETE-CARDEX-REC THRU WRITE-CARDEX-EXIT
		   MOVE 01	 TO CRD-SMAN
		   PERFORM WRITE-CARDEX THRU WRITE-CARDEX-EXIT.
	     GO TO BA15.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	     Error Message - Invalid Reference number	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       BA20.
	     MOVE "Reference Number"
				 TO WS-ERR-STRING.
003260	     PERFORM ERROR-MESSAGE.
	     GO TO BA10.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ End of the current Invoice - Clear reference and get next ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       BA25.
	     MOVE ZERO		 TO W10-NUM.
	     GO TO BA10.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		 Extract of Statistics completed.	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       BA30.
	     PERFORM RESTORE-SCREEN-3.
	     DISPLAY "Printing statistics:" AT 0604.
	     MOVE SPACES	 TO EXT-STK-CODE
				    WS-EXT-STK
				    EXT-AC.
	     MOVE ZERO		 TO EXT-DTE
				    EXT-SEQ.
006090	     START SALEXT
	       KEY >= EXT-KEY.
006100     IF WS-STATUS = "00"
	       MOVE ZERO	 TO WS-F-ERROR
      *
      *    ****    Open Printer, set up variables and print headings
      *
	       PERFORM BA100 THRU BA115
006110	       GO TO BA35.
	     MOVE "No Data extracted"
				 TO WS-ERR-STRING.
003260	     PERFORM ERROR-MESSAGE.
	     GO TO BA999.

       BA35.
006090	     READ SALEXT NEXT.
006100     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110	       GO TO BA40.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
	       MOVE 93		 TO WS-F-ERROR
	       GO TO BA40.
006120     IF WS-STAT1 = "2" OR "3" OR "4"
006130	       MOVE 93		 TO WS-F-ERROR
006140         PERFORM READ-ERROR.
006150     IF RECORD-LOCKED
006160         PERFORM LOCKED-RECORD
	       GO TO BA35.
      *
      *    ****    E N D   O F	 F I L E   ?
      *
       BA40.
	   IF WS-F-ERROR = 93
	       PERFORM BA50
	       IF WS-LOOP = "C"
		   MOVE ZERO	 TO WS-PAGE
		   MOVE "S"	 TO WS-LOOP
		   MOVE SPACES	 TO EXT-STK-CODE
				    WS-EXT-STK
				    EXT-AC
		   MOVE ZERO	 TO EXT-DTE
				    EXT-SEQ
006090		   START SALEXT
		       KEY >= EXT-KEY
		   PERFORM BA110 THRU BA115
		   GO TO BA35
	       END-IF
	       CLOSE PRNREP
	       GO TO BA55.
	     DISPLAY EXT-STK-CODE AT 0626 WITH FOREGROUND-COLOR 14
		     " " EXT-AC WITH FOREGROUND-COLOR 15.
      *
      *    ****   S T I L L   B U S Y	W I T H   S A M E   I T E M   ?
      *
	   IF EXT-STK-CODE = WS-EXT-STK
	       GO TO BA45.
      *
      *    ****   I T E M   C O D E   C H A N G E D
      *
	     MOVE EXT-STK-CODE	 TO WS-EXT-STK
				    STK-CODE.
	     PERFORM READ-STOCK THRU READ-STOCK-EXIT.
	   IF LINAGE-COUNTER > 60
	       MOVE SPACES	 TO REP-DETAIL1
	       WRITE REP-LINE1 BEFORE PAGE
	       PERFORM BA110.
	     MOVE STK-CODE	 TO ST-CODE.
	     MOVE STK-DESC	 TO ST-DESC.
	     WRITE REP-LINE1 BEFORE 1 LINES.
	     MOVE SPACES	 TO REP-DETAIL1.

       BA45.
	     MOVE EXT-AC	 TO ST-AC.
	     MOVE EXT-REF	 TO ST-REFER.
	     MOVE EXT-QNT	 TO ST-QUANT.
	   IF WS-LOOP = "C"
	       MOVE EXT-COST	 TO ST-COST
	       MULTIPLY EXT-COST BY EXT-QNT
				 GIVING W25-VALUE
	       MOVE W25-VALUE	 TO ST-TOTAL
	       ADD W25-VALUE	 TO W25-TOTAL
	   ELSE
	       MOVE EXT-SELL	 TO ST-COST
	       MULTIPLY EXT-SELL BY EXT-QNT
				 GIVING W25-VALUE
	       MOVE W25-VALUE	 TO ST-TOTAL
	       ADD W25-VALUE	 TO W25-TOTAL.
	   IF LINAGE-COUNTER < 62
	       WRITE REP-LINE1 BEFORE 1 LINES
	   ELSE
	       WRITE REP-LINE1 BEFORE PAGE
	       PERFORM BA110
	       MOVE STK-CODE	 TO ST-CODE
	       MOVE STK-DESC	 TO ST-DESC
	       WRITE REP-LINE1 BEFORE 1 LINES.
	     MOVE SPACES	 TO REP-DETAIL1.
	     GO TO BA35.

       BA50.
	     WRITE REP-LINE1 BEFORE 1.
	   IF WS-LOOP = "C"
	       MOVE "Total Cost:"
				 TO ST-DESC
	   ELSE
	       MOVE "Total Selling:"
				 TO ST-DESC.
	     MOVE W25-TOTAL	 TO ST-TOTAL.
	     WRITE REP-LINE1 BEFORE 0 LINES.
	     WRITE REP-LINE1 BEFORE PAGE.
	     MOVE SPACES TO REP-DETAIL1.
002900	     MOVE ZERO		 TO W25-TOTAL
				    W25-VALUE.

       BA55.
	     DISPLAY "Amending VAT Details:" AT 0804.
      *
      *    ****    V A T   T R A N S A C T I O N   D A T E
      *
	     INITIALIZE W10-TAX-KEY.
	     MOVE W10-TAX-KEY	 TO TAX-KEY.
      *
      *    TYPE: "1" = Input Tax
      *
	     MOVE "1"		 TO TAX-TYPE.
	     MOVE "0"		 TO W10-TAX-TYPE.
	     PERFORM START-AT-TXTRAN-KEY THRU READ-TXTRAN-EXIT.
	   IF WS-F-ERROR = 32
	       GO TO BA999.

       BA60.
	     PERFORM READ-TXTRAN-NEXT THRU READ-TXTRAN-EXIT.
	   IF WS-F-ERROR = 32
	       GO TO BA999.
	     DISPLAY TAX-DATE AT 0826 WITH FOREGROUND-COLOR 14
		     " " TAX-REF WITH FOREGROUND-COLOR 15.
	   IF TAX-DATE = 20040415
	       PERFORM DELETE-TXTRAN-REC THRU WRITE-TXTRAN-EXIT
	       MOVE 20050415	 TO TAX-DATE
	       PERFORM WRITE-TXTRAN THRU WRITE-TXTRAN-EXIT.
	     GO TO BA60.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		    Statistic report routines.		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       BA100.
	     MOVE "LPT1"	 TO W02-PRINTER.
      *      MOVE "DTPSTA"	 TO W02-PRINTER.
	     MOVE ZERO		 TO WS-PRN-NO.
	     MOVE ZERO		 TO WS-PAGE.
002900	     MOVE ZERO		 TO W25-TOTAL
				    W25-VALUE.

       CHECK-PRINTER.
	     MOVE ZERO		 TO WS-ERROR
				    WS-S1 WS-S2 WS-S3 WS-S4
				    WS-S5 WS-S6 WS-S7 WS-S8.
	     CALL "PC_TEST_PRINTER" USING WS-PRN-NO
					  WS-PRN-STAT
				    RETURNING WS-STATUS.
	   IF WS-PRN-STAT > 127
	       MOVE 1		 TO WS-S1
	       SUBTRACT 128	 FROM WS-PRN-STAT.
	   IF WS-PRN-STAT > 63
	       MOVE 1		 TO WS-S2
	       SUBTRACT 64	 FROM WS-PRN-STAT.
	   IF WS-PRN-STAT > 31
	       MOVE 1		 TO WS-S3
	       SUBTRACT 32	 FROM WS-PRN-STAT.
	   IF WS-PRN-STAT > 15
	       MOVE 1		 TO WS-S4
	       SUBTRACT 16	 FROM WS-PRN-STAT.
	   IF WS-PRN-STAT > 7
	       MOVE 1		 TO WS-S5
	       SUBTRACT 8	 FROM WS-PRN-STAT.
	   IF WS-PRN-STAT > 3
	       MOVE 1		 TO WS-S6
	       SUBTRACT 4	 FROM WS-PRN-STAT.
	   IF WS-PRN-STAT > 1
	       MOVE 1		 TO WS-S7
	       SUBTRACT 2	 FROM WS-PRN-STAT.
	     MOVE WS-PRN-STAT	 TO WS-S8.
	   IF WS-S3 = 0 AND
	      WS-S5 = 0 AND
	      WS-S8 = 0
	       GO TO CHECK-PRINTER-EXIT.

       PRINTER-NOT-READY.
	     MOVE "Check printer! - 'R'etry or 'A'bort  [ ] "
				 TO WS-ERR-MES.
	     PERFORM OPT-MESSAGE.
	   IF NOT (WS-OPTION = "R" OR "A")
	       GO TO PRINTER-NOT-READY.
	     IF WS-OPTION = "A"
		 MOVE 1 	 TO WS-ERROR
		 GO TO CHECK-PRINTER-EXIT.
	       GO TO CHECK-PRINTER.

       CHECK-PRINTER-EXIT.
	     EXIT.

       BA105.
019890	       OPEN OUTPUT PRNREP.

       BA110.
	     MOVE SPACES	 TO REP-DETAIL1.
	     MOVE "Date:"	 TO REP-DATE.
	     MOVE TODAY-DDMMYY	 TO REP-DTE.
	     MOVE "Page:"	 TO REP-PAGE.
	     ADD 1		 TO WS-PAGE.
	     MOVE WS-PAGE	 TO REP-P-NO.
	     MOVE W95-COMP	 TO REP-COMPANY.
	     WRITE REP-LINE1 BEFORE 1 LINES.
	     MOVE SPACES	 TO REP-DETAIL1.
	   IF WS-LOOP = "C"
	       MOVE "Statistics Report of Items at cost"
				 TO REP-COMPANY
	   ELSE
	       MOVE "Statistics Report of Items Invoiced"
				 TO REP-COMPANY.
	     WRITE REP-LINE1 BEFORE 1 LINES.
	     MOVE ALL "Í"	 TO REP-DETAIL1.
	     WRITE REP-LINE1 BEFORE 1 LINES.
	     MOVE SPACES	 TO REP-DETAIL1.
	     MOVE "Item Code"	 TO ST-H1.
	     MOVE "Description"  TO ST-H2.
	     WRITE REP-LINE1 BEFORE 1 LINES.
	     MOVE SPACES	 TO REP-DETAIL1.
	     MOVE "AC.No."	 TO ST-H3.
	     MOVE "Reference"	 TO ST-H4.
	     MOVE "Quantity"	 TO ST-H5.
	   IF WS-LOOP = "C"
	       MOVE "Unit Cost"  TO ST-H6
	       MOVE "Total Cost" TO ST-H7
	   ELSE
	       MOVE "Unit Sell"  TO ST-H6
	       MOVE "Total Sell" TO ST-H7.
	     WRITE REP-LINE1 BEFORE 1 LINES.
	     MOVE ALL "Í"	 TO REP-DETAIL1.
	     WRITE REP-LINE1 BEFORE 2 LINES.
	     MOVE SPACES	 TO REP-DETAIL1.

       BA115.
	     EXIT.

021720 BA999.
021730       EXIT.
      /
      *    ****    I N I T I A L I Z E   P R O G R A M
      *   
041980 ZA000-INIT	       SECTION 80.
041990 ZA000-OPEN.
	     ACCEPT W25-FACT FROM DATE.
	     MOVE W25-YY	 TO W20-YEAR  W12-YEAR.
	     MOVE W25-MM	 TO W20-MONTH W12-MONTH.
	     MOVE W25-DD	 TO W20-DAY   W12-DAY.
	     MOVE 20		 TO W20-CENT  W12-CENT.
	   IF W25-YY < 50
	       MOVE 20		 TO W12-CENT
	   ELSE
	       MOVE 19		 TO W12-CENT.
	     MOVE W12-TODAY	 TO TODAY-DDMMYY.
003250	     MOVE W12-DAY	 TO W12-DD.
	     MOVE W12-MONTH	 TO W12-MM.
003260	     MOVE W12-CENT	 TO W12-CC.
003270	     MOVE W12-YEAR	 TO W12-YY.
	     MOVE "APAC Utility" TO APAC-SYSTEM.
	     MOVE "8.11"	 TO APAC-VERSION.
	     PERFORM ZA55 THRU ZA60.
042000	     DISPLAY CLR-SCREEN.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ D o   n o t   d i s p l a y   E R R O R   m e s s a g e s ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE 44		 TO USER-NUMBER.
	     MOVE 1		 TO USER-SETTING.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
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
	       MOVE 66		 TO MOUSE-FUNC
	       MOVE 0		 TO MOUSE-PARAM
	       CALL X"AF" USING MOUSE-FUNC
				MOUSE-PARAM
	   ELSE
	       GO TO ZA00A.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	       Set MOUSE key to act as FUNCTION key	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE 3		 TO USER-ACTION.
	     MOVE 27		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
	     DISPLAY "PATCH DEBTOR TRAN DATES ECT" AT 0228
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" AT 1126
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7.
	     DISPLAY "³ Company/Business Code     ³" AT 1226
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7.
	     DISPLAY "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" AT 1326
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7.
	     ACCEPT WS-SYS-ID AT 1250
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	   IF WS-SYS-ID = SPACES
	       MOVE 9		 TO WS-ERROR
	       GO TO ZA999-EXIT.
	     MOVE WS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ This is the file-id of the file used to accumulate the    ³
      *    ³ statistics of all the reference numbers that are keyed in ³
      *    ³ to the program by the User.			       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
				    W02-PTH43B.
	     OPEN I-O APACFIDS.
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
      *    ****        D E B T O R   I N V O I C E   F I L E	    ****
      *    *************************************************************
	     MOVE "INVOIC"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-INVOIC.
      *    *************************************************************
      *    **** 		S T O C K   F I L E		    ****
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

042170 ZA02.
      *    *************************************************************
      *    ****       S A L E S	  -   C A R D E X   F I L E	    ****
      *    *************************************************************
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
053920		   PERFORM OPEN-ERROR.
      *    *************************************************************
      *    ****        D E B T O R   I N V O I C E   F I L E	    ****
      *    *************************************************************
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
054230             GO TO ZA50.
054240     IF WS-STATUS NOT = "00"
054250         MOVE 12           TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
054260		   PERFORM OPEN-ERROR.
      *    *************************************************************
      *    **** 	    P A R A M E T E R	F I L E		    ****
      *    *************************************************************
	     OPEN I-O PARAM.
053840	   IF RUNTIME-ERROR
053850         IF FLE-LOCKED
053860             GO TO ZA200
053870         ELSE
053880         IF FLE-LIMIT
053890		   GO TO ZA49
               ELSE
	       IF IDX-CORRUPT
		   MOVE 15	 TO WS-F-ERROR
		   GO TO ZA50.
053900     IF WS-STATUS NOT = "00"
053910	       MOVE 15		 TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
053920		   PERFORM OPEN-ERROR.
      *    *************************************************************
      *    ****       E X T R A C T E D	  S T A T S   F I L E	    ****
      *    *************************************************************
	     OPEN OUTPUT SALEXT
	     CLOSE SALEXT
	     OPEN I-O SALEXT.
      *    *************************************************************
      *    **** 		S T O C K   F I L E		    ****
      *    *************************************************************
053830	     OPEN I-O STOCK.
053840	   IF RUNTIME-ERROR
053850         IF FLE-LOCKED
053860             GO TO ZA200
053870         ELSE
053880         IF FLE-LIMIT
053890		   GO TO ZA49
               ELSE
	       IF IDX-CORRUPT
		   MOVE 22	 TO WS-F-ERROR
		   GO TO ZA50.
053900     IF WS-STATUS NOT = "00"
053910	       MOVE 22		 TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
044350		   PERFORM OPEN-ERROR.
      *    *************************************************************
      *    ****       V A L U E	  A D D E D   T A X   F I L E	    ****
      *    *************************************************************
053830	     OPEN I-O TXTRAN.
	   IF WS-STATUS = "05"
	       CLOSE TXTRAN
	       OPEN OUTPUT TXTRAN
	       CLOSE TXTRAN
	       OPEN I-O TXTRAN.
053840	   IF RUNTIME-ERROR
053850         IF FLE-LOCKED
053860             GO TO ZA200
053870         ELSE
053880         IF FLE-LIMIT
053890		   GO TO ZA49
               ELSE
	       IF IDX-CORRUPT
		   MOVE 32	 TO WS-F-ERROR
		   GO TO ZA50.
053900     IF WS-STATUS NOT = "00"
053910	       MOVE 32		 TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
044350		   PERFORM OPEN-ERROR.
042720	     MOVE 1		 TO WS-PARKEY.
042730       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
042740       MOVE PAR-COMPANY    TO W95-COMP.
042750       MOVE PAR-DMY        TO W12-TODAY.
042760       MOVE PAR-YMD        TO W12-T-YMD.
044780	     GO TO ZA999-EXIT.

039780 ZA49.
039790       DISPLAY "Too many files OPEN" AT 0812
039800                WITH FOREGROUND-COLOR 14.
039810       DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
039820       DISPLAY "Press any key to continue" AT 1212.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
039840	     GO TO ZA205.
039850 ZA50.
055350	   IF WS-F-ERROR = 12
055360         MOVE "INVOICE file"
055370				 TO WS-FILE
055380	   ELSE
055350	   IF WS-F-ERROR = 15
055360	       MOVE "PARAMETER file"
055370				 TO WS-FILE
055380	   ELSE
055350	   IF WS-F-ERROR = 22
055360	       MOVE "STOCK file"
055370				 TO WS-FILE
055380	   ELSE
055350	   IF WS-F-ERROR = 32
055360	       MOVE "VAT file"	 TO WS-FILE
	   ELSE
041850	   IF WS-F-ERROR = 43
041860	       MOVE "CARDEX (SALES)"
				 TO WS-FILE.
039980	     DISPLAY "Rebuild " AT 0812
039990                WS-FILE WITH FOREGROUND-COLOR 14.
040000       DISPLAY "Press any key to continue" AT 1012.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
040020 ZA51.
040030       EXIT PROGRAM.

       ZA55.
041220       MOVE 1              TO WS-S1.
041240 ZA60.
041250       MOVE WS-G1          TO WS-TCHR (WS-S1) WS-BCHR (WS-S1).
             MOVE WS-G8          TO WS-TCH  (WS-S1) WS-BCH  (WS-S1).
041260     IF WS-S1 < 80
041270         ADD 1             TO WS-S1
041280         GO TO ZA60.
             MOVE WS-G9          TO WS-TCH  (1).
             MOVE WS-G10         TO WS-TCH  (80).
             MOVE WS-G11         TO WS-BCH  (1).
             MOVE WS-G12         TO WS-BCH  (80).
             MOVE WS-G14         TO WS-TCHR (1)  WS-BCHR (1).
             MOVE WS-G13         TO WS-TCHR (80) WS-BCHR (80).
021930       MOVE WS-G2          TO WS-TCHR (8)  WS-TCHR (49)
021940                              WS-TCHR (61) WS-TCHR (72).
021970       MOVE WS-G4          TO WS-BCHR (8)  WS-BCHR (49)
021980                              WS-BCHR (61) WS-BCHR (72).
             MOVE WS-G13         TO WS-TCHR (80) WS-BCHR (80).
             MOVE WS-G14         TO WS-TCHR (1)  WS-BCHR (1).
	     MOVE "SupervisorÄ"  TO WS-WRKHD.

042800 ZA200.
042810       DISPLAY "Files locked - Try later" AT 2312
042820                WITH FOREGROUND-COLOR 14
                     " " WS-STATUS WITH FOREGROUND-COLOR 15.
042830       DISPLAY "Press any key" AT 2512 
042840		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
042850	     ACCEPT WS-OPTION AT 2526
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.

       ZA205.
             EXIT PROGRAM.

042880 ZA999-EXIT.
042890       EXIT.
      /
      *    ****    I / O   E R R O R   M E S S A G E S
      *   
042900 ZB000-ERROR	     SECTION 81.
      
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
       I-O-ERROR-EXIT.
             EXIT.

041560 DISPLAY-FILE-NAME.
056470	   IF WS-F-ERROR = 12
056480         MOVE W02-INVOIC   TO WS-FILE
056490         MOVE ZERO         TO WS-KEY
	       MOVE DOC-KEY	 TO WS-KEYX
041110	   ELSE
041120     IF WS-F-ERROR = 15
041130	       MOVE W02-PARAM	 TO WS-FILE
041140	       MOVE WS-PARKEY	 TO WS-KEY
	   ELSE
052200	   IF WS-F-ERROR = 22
052210         MOVE W02-STOCKF   TO WS-FILE
052220         MOVE ZERO         TO WS-KEY
               MOVE STK-CODE     TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 32
041860	       MOVE W02-TXTRAN	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE TAX-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 43
041860	       MOVE W02-CARDEX	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE CRD-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 93
041860	       MOVE W02-SALEXT	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE EXT-KEY	 TO WS-KEYX.
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
