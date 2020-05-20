      $SET LINKCOUNT"640"
      ******************************************************************
      *                                                                *
      * *******  ******** *******  *******  ********  *****   **    ** *
      * **    **    **	  **	** **	 **    **    **   **  **    ** *
      * **    **    **	  **	** **	 **    **    **       **    ** *
      * **    **    **	  *******  *******     **    **       ******** *
      * **    **    **	  **	   **	       **    **       **    ** *
      * **    **    **	  **	   **	       **    **   **  **    ** *
      * *******     **	  **	   **	       **     *****   **    ** *
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
      *  ³ time and is used to correct or amend details in the Debtor  ³
      *  ³ transaction, Cardex (sales, purchases and adjustment) and   ³
      *  ³ Invoice files.					       ³
      *  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     DTPPTCH.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   March 1993.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1993 - 2011
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

000140 COPY DBTRAN.SL.

000250 COPY INVOIC.SL.

       COPY PURDEX.SL.

       COPY STKDEX.SL.

000260 DATA DIVISION.
000270 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CARDEX.FDE.

000420 COPY DBTRAN.FDE.

000540 COPY INVOIC.FDE.

       COPY PURDEX.FDE.

       COPY STKDEX.FDE.

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
002780 77  WS-LOOK	   PIC	X(01) VALUE "S".
001820 77  WS-OPTION       PIC  X(01).
       77  WS-USE-ITM	   PIC	X(01).
       77  WS-EXT-STK	   PIC	X(01).
001820 77  WS-ENQ	   PIC	X(01).
       77  WS-MONTH	   PIC	9(02).
       77  WS-TCODE	   PIC	9(02).
001820 77  WS-ETYPE	   PIC	X(01).
001850 77  WS-ERROR        PIC  9(01) VALUE ZERO.
       77  WS-USUB	   PIC	9(04) COMP-5.

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
	   03  WS-PROG	       PIC  X(04) VALUE "PTCH".

002310 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CARDEX.ID.

000140 COPY DBTRAN.ID.

000140 COPY INVOIC.ID.

       COPY PARAM.ID.

       COPY PURDEX.ID.

       COPY STKDEX.ID.

       01  WS-APAC.
	   03  APAC-SYSTEM  PIC  X(12).
	   03  APAC-VERSION PIC  X(04).

002500 01  W10-SPARES.
002510	   03  W10-DATE	    PIC X(10).
002520	   03  W10-EDTE REDEFINES W10-DATE PIC 99/99/9999.

       01  W10-REF.
           03  W10-NUM         PIC  9(08).

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

      /
004580 SCREEN SECTION.

       COPY BLANK.CRT.

009340 01  S05D.
	   03  LINE  4 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6
			      VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
	   03  LINE  5 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6
			      VALUE "³    Amend Incorrect Dates.    ³".
	   03  LINE  6 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6
			      VALUE "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".
	   03  LINE  7 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6
			      VALUE "³ Target Date    :             ³".
	   03  LINE  8 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6
			      VALUE "³ Replacement Date:            ³".
	   03  LINE  9 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6
			      VALUE "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".
	   03  LINE 10 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6
			      VALUE "³Dates to be keyed in: ".
	   03	       COLUMN 48 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE "DDMMCCYY".
	   03	       COLUMN 56 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE "³".
	   03  LINE 11 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6
      *			      VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".
      *
      *   Dates must only be changed for entries that have a reference
      *   number that is not less than the reference number that has
      *   keyed in.
      *
			      VALUE "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".
	   03  LINE 12 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6
			      VALUE "³ Start Reference :            ³".
	   03  LINE 13 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6
			      VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S05A.
	   03  LINE  7 COLUMN 45 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC 99/99/9999 USING W15-START AUTO.
	   03  LINE  8 COLUMN 45 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC 99/99/9999 USING W15-END AUTO.
	   03  LINE 12 COLUMN 45 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZZZZZZ9 USING W10-NUM AUTO.

004870 COPY S99.CRT.

       COPY ERROR.CRT.

      /
007730 PROCEDURE DIVISION.
010250 AA000-MAIN	       SECTION.
010260 AA000-INIT.
010270       PERFORM ZA000-INIT.
	     PERFORM BA000.
	     CLOSE CARDEX
		   DBTRAN
		   INVOICE
		   PURDEX
		   STKDEX.

       AA49.
013150	     STOP RUN.

       COPY FUNCTION.CRT.

011390 COPY LOCKED.RC2.

011410 COPY CLEAR.CRT.

       COPY DATE.CHK.

      /
      *    ****    READ FILES ROUTINES
      *
011650 AC000              SECTION.

       COPY CARDEX.RD.

000140 COPY DBTRAN.RD.

       COPY INVOIC.RD.

       COPY PURDEX.RD.

       COPY STKDEX.RD.

      /
      *    ****    WRITE FILES ROUTINES
      *
011650 AD000		  SECTION.

       COPY CARDEX.WR.

000140 COPY DBTRAN.WR.

       COPY INVOIC.WR.

       COPY PURDEX.WR.

       COPY STKDEX.WR.

       COPY APAC.HLP.

      /
      *  ****  A M E N D   D A T E S
      *
021000 BA000		       SECTION 50.
021010 BA00.
021020       DISPLAY CLR-SCREEN.
	     DISPLAY "AMEND INCORRECT DATES" AT 0226
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.

       BA05.
	     PERFORM SAVE-SCREEN-3.
	     MOVE 5		 TO SHADE-ROW.
	     MOVE 27		 TO SHADE-COL.
	     MOVE 30		 TO SHADE-WIDTH.
	     MOVE 9		 TO SHADE-LINES.
	     DISPLAY S05D.
	     PERFORM SCREEN-SHADOW.
	     MOVE W12-TODAY	 TO W15-START W15-END.
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
		     GO TO BA999
		 WHEN OTHER	 CALL X"E5"
		     GO TO BA10
	       END-EVALUATE.
	     PERFORM CHECK-CORRECT.
020250	   IF WS-OPTION = "N"
	       GO TO BA10.
	     DISPLAY CLEAR-L25.
	     MOVE W15-START	 TO W10-EDTE.
             PERFORM CHECK-DATE.
027400     IF WS-ERROR = 1
027410	       MOVE "Target date"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BA10.
             MOVE W20-DTE        TO W15-S-YMD.
	     MOVE W15-END	 TO W10-EDTE.
             PERFORM CHECK-DATE.
027400     IF WS-ERROR = 1
027410	       MOVE "Replacement date"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BA10.
             MOVE W20-DTE        TO W15-E-YMD.
	   IF W10-NUM = ZERO
	       MOVE "Reference Number"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BA10.
	     PERFORM RESTORE-SCREEN-3.
      *
      *    ****   I N V O I C E S
      *
	    MOVE 01		 TO WS-TCODE.
      *
      *    ****   C R E D I T	N O T E S
      *
      *     MOVE 04		 TO WS-TCODE.
	     DISPLAY "Amending Debtor Transaction:" AT 0604.
      *
      *    ****    D E B T O R	 T R A N S A C T I O N	 D A T E
      *
018910 BA15.
	     MOVE WS-TCODE	 TO TRA-CODE.
	     MOVE W15-S-YMD	 TO TRA-DATE.
	     PERFORM START-AT-DTRN-CODE THRU READ-DBTRAN-EXIT.
	   IF WS-F-ERROR = 5
	       GO TO BA25.

       BA20.
	     PERFORM READ-DBTRAN-NEXT THRU READ-DBTRAN-EXIT.
	   IF WS-F-ERROR = 5
	       GO TO BA25.
	   IF NOT (TRA-CODE = WS-TCODE)
	       GO TO BA25.
	   IF NOT (TRA-DATE = W15-S-YMD)
	       GO TO BA25.
      *
      *    ****   L O O K   F O R   S T A R T I N G   R E F
      *
	   IF WS-TCODE = 01 OR 04
	       IF TRA-REF < W10-NUM
		   GO TO BA20.
	     DISPLAY TRA-KEY AT 0633
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT.
	     PERFORM DELETE-DBTRAN-REC THRU WRITE-DBTRAN-EXIT.
	     MOVE W15-E-YMD	 TO TRA-DATE.
	     PERFORM WRITE-DBTRAN THRU WRITE-DBTRAN-EXIT.
	     GO TO BA15.

       BA25.
	   IF WS-TCODE < 04
	       IF WS-TCODE = 01
		   ADD 1	 TO WS-TCODE
	       ELSE
		   MOVE 04	 TO WS-TCODE
	       END-IF
	       GO TO BA15.
	     DISPLAY "Amending Sales Tracking:" AT 0804.
      *
      *    ****    C A R D E X	 D A T E
      *
       BA30.
	     MOVE SPACES	 TO CRD-EXT-CODE.
	     MOVE W15-S-YMD	 TO CRD-DTE.

       BA35.
000750	     PERFORM START-AT-CRDX-DSTK THRU READ-CARDEX-EXIT.
000880	   IF WS-F-ERROR = 43
	       GO TO BA45.

       BA40.
000840	     PERFORM READ-CARDEX-NEXT THRU READ-CARDEX-EXIT.
	   IF WS-F-ERROR = 43
	       GO TO BA45.
	   IF NOT (CRD-DTE = W15-S-YMD)
	       GO TO BA45.
      *
      *    ****   L O O K   F O R   S T A R T I N G   R E F
      *
	   IF CRD-REF < W10-REF
	       GO TO BA40.
	     DISPLAY CRD-KEY AT 0829
		     WITH FOREGROUND-COLOR 4 HIGHLIGHT.
	     PERFORM DELETE-CARDEX-REC THRU WRITE-CARDEX-EXIT.
	     MOVE W15-E-YMD	 TO CRD-DTE.
	     PERFORM WRITE-CARDEX THRU WRITE-CARDEX-EXIT.
	     GO TO BA30.
      *
      *    ****    P U R D E X	 D A T E
      *
       BA45.
      *
      *    ****    S K I P   P U R C H A S E   &   A D J U S T M E N T
      * 	   T R A C K I N G   T H I S   T I M E
      *
	     GO TO BA75.
       BA46.
	     DISPLAY "Amending Purchase Tracking:" AT 1004.
	     MOVE SPACES	 TO PRD-EXT-CODE.
	     MOVE W15-S-YMD	 TO PRD-DTE.

       BA50.
000750	     PERFORM START-AT-PRDX-SDTE THRU READ-PURDEX-EXIT.
000880	   IF WS-F-ERROR = 56
	       GO TO BA60.

       BA55.
000840	     PERFORM READ-PURDEX-NEXT THRU READ-PURDEX-EXIT.
	   IF WS-F-ERROR = 56
	       GO TO BA60.
	   IF NOT (PRD-DTE = W15-S-YMD)
	       GO TO BA60.
	     DISPLAY PRD-KEY AT 1032
		     WITH FOREGROUND-COLOR 6 HIGHLIGHT.
	     PERFORM DELETE-PURDEX-REC THRU WRITE-PURDEX-EXIT.
	     MOVE W15-E-YMD	 TO PRD-DTE.
	     PERFORM WRITE-PURDEX THRU WRITE-PURDEX-EXIT.
	     GO TO BA45.
      *
      *    ****    S T K D E X	 D A T E
      *
       BA60.
	     DISPLAY "Amending Adjustment Tracking:" AT 1204.
	     MOVE SPACES	 TO STX-EXT-CODE.
	     MOVE W15-S-YMD	 TO STX-DTE.

       BA65.
000750	     PERFORM START-AT-STDX-DATE THRU READ-STKDEX-EXIT.
000880	   IF WS-F-ERROR = 46
	       GO TO BA75.

       BA70.
000840	     PERFORM READ-STKDEX-NEXT THRU READ-STKDEX-EXIT.
	   IF WS-F-ERROR = 46
	       GO TO BA75.
	   IF NOT (PRD-DTE = W15-S-YMD)
	       GO TO BA75.
	     DISPLAY STX-KEY AT 1234
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT.
	     PERFORM DELETE-STKDEX-REC THRU WRITE-STKDEX-EXIT.
	     MOVE W15-E-YMD	 TO STX-DTE.
	     PERFORM WRITE-STKDEX THRU WRITE-STKDEX-EXIT.
	     GO TO BA60.
      *
      *    ****    I N V O I C E   D A T E
      *
       BA75.
	     DISPLAY "Amending Debtor Invoices:" AT 1404.
	     MOVE SPACES	 TO DOC-REF.
	     MOVE ZERO		 TO DOC-SQN.
	     MOVE W15-S-YMD	 TO DOC-DTE.

       BA80.
	    PERFORM START-AT-INVOIC-DATE THRU READ-INVOIC-EXIT.
000880	   IF WS-F-ERROR = 12
	       GO TO BA999.

       BA85.
	    PERFORM READ-INVOIC-NEXT THRU READ-INVOIC-EXIT.
000880	   IF WS-F-ERROR = 12
	       GO TO BA999.
	   IF NOT (DOC-DTE = W15-S-YMD)
	       GO TO BA999.
	     DISPLAY DOC-KEY AT 1430
		     WITH FOREGROUND-COLOR 5 HIGHLIGHT.
	     MOVE W15-E-YMD	 TO DOC-DTE.
	     PERFORM REWRITE-INVOIC THRU WRITE-INVOIC-EXIT.
	     GO TO BA75.

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
	     MOVE WS-SYS-ID	 TO W02-SYSID.
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
	     MOVE "CARDEX"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CARDEX.
	     MOVE "DBTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DBTRAN.
	     MOVE "INVOIC"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-INVOIC.
	     MOVE "PURDEX"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-PURDEX.
	     MOVE "STKDEX"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STKDEX.

042170 ZA02.
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
053830	     OPEN I-O STKDEX.
	   IF WS-STATUS = "05"
	       CLOSE STKDEX
	       OPEN OUTPUT STKDEX
	       CLOSE STKDEX
	       OPEN I-O STKDEX.
053840	   IF RUNTIME-ERROR
053850         IF FLE-LOCKED
053860             GO TO ZA200
053870         ELSE
053880         IF FLE-LIMIT
053890		   GO TO ZA49
               ELSE
	       IF IDX-CORRUPT
		   MOVE 46	 TO WS-F-ERROR
		   GO TO ZA50.
053900     IF WS-STATUS NOT = "00"
053910	       MOVE 46		 TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
044350		   PERFORM OPEN-ERROR.
044780	     GO TO ZA999-EXIT.

039780 ZA49.
039790       DISPLAY "Too many files OPEN" AT 0812
039800                WITH FOREGROUND-COLOR 14.
039810       DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
039820       DISPLAY "Press any key to continue" AT 1212.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
039840	     GO TO ZA205.
039850 ZA50.
041850	   IF WS-F-ERROR = 5
041860         MOVE "DEBTOR TRAN"
                                 TO WS-FILE
055380	   ELSE
055350     IF WS-F-ERROR = 12
055360         MOVE "INVOICE file"
055370                           TO WS-FILE
	   ELSE
041850	   IF WS-F-ERROR = 43
041860	       MOVE "CARDEX (SALES)"
				 TO WS-FILE
	   ELSE
041850	   IF WS-F-ERROR = 46
041860	       MOVE "CARDEX (ADJUST)"
				 TO WS-FILE
	   ELSE
041850	   IF WS-F-ERROR = 56
041860	       MOVE "CARDEX (PURCH)"
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
056350	   IF WS-F-ERROR = 5
056360         MOVE W02-DBTRAN TO WS-FILE
041750         MOVE ZERO         TO WS-KEY
               MOVE TRA-KEY      TO WS-KEYX
056460	   ELSE
056470     IF WS-F-ERROR = 12
056480         MOVE W02-INVOIC   TO WS-FILE
056490         MOVE ZERO         TO WS-KEY
               MOVE DOC-KEY      TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 43
041860	       MOVE W02-CARDEX	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE CRD-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 46
041860	       MOVE W02-STKDEX	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE STX-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 56
041860	       MOVE W02-PURDEX	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE PRD-KEY	 TO WS-KEYX.
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
