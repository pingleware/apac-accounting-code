      ******************************************************************
      *                                                                *
      *   ****	  **	 **  ******    ******  ********	****  ******** *
      *  **  **   ***	***  **	  **  **    **	 **    **  **	 **    *
      * **    **  ** * * **  **	  **  **	 **   **    **	 **    *
      * ********  **  *  **  **   **   ******	 **   ********	 **    *
      * **    **  **	 **  **	  **	    **	 **   **    **	 **    *
      * **    **  **	 **  **	  **  **    **	 **   **    **	 **    *
      * **    **  **	 **  ******    ******	 **   **    **	 **    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *  A M E N D   D E B T O R   S T A T I S T I C S	 P R O G R A M *
      *                                                                *
      *     VERSION 7.02 - January 2000 			       *
      * 							       *
      ******************************************************************
      /
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     AMDSTAT.
000030 AUTHOR.         J.W.LEMMON (APAC).
000040 DATE-WRITTEN.   AUGUST 1982.
000050 SECURITY.       NONE.
000060 ENVIRONMENT DIVISION.
000070 CONFIGURATION SECTION.
000080 SPECIAL-NAMES.
000080		       CURSOR IS CSTART
000090		       CONSOLE IS CRT
		       CRT STATUS IS KEY-STATUS.
000100 INPUT-OUTPUT SECTION.
000110 FILE-CONTROL.

       COPY APACFIDS.SL.

000130 COPY CONTROL.SL.

000190 COPY DEPART.SL.

000210 COPY PARAM.SL.

000490 DATA DIVISION.
000500 FILE SECTION.

       COPY APACFIDS.FD.

000520 COPY CONTROL.IFD.

000520 COPY DEPART.GFD.

000520 COPY PARAM.IFD.

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
003240 77  WS-S1	   PIC	9(04) COMP-5.
003250 77  WS-S2	   PIC	9(04) COMP-5.
003260 77  WS-S3	   PIC	9(04) COMP-5.
003270 77  WS-S4	   PIC	9(04) COMP-5.
003280 77  WS-S5	   PIC	9(04) COMP-5.
003290 77  WS-S6	   PIC	9(04) COMP-5.

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
000020	   03  WS-PTH15	   PIC	X(03).
000030	   03  FILLER	   PIC	X(15) VALUE "_DATA\PARAM.DAT".

003900 01  W02-FID.

       COPY CONTROL.ID.

       COPY DEPART.ID.

       01  W15-DEBTOR-TRAN.
001640	   03  W15-E-DESC  PIC	X(12).
001690	   03  W15-T-DAY   PIC	9(06)	  COMP-5.
001700	   03  W15-T-VAL   PIC S9(09)V99  COMP-3.
001710	   03  W15-T-MTD   PIC	9(06)	  COMP-5.
001720	   03  W15-T-MTDV  PIC S9(09)V99  COMP-3.
001730	   03  W15-T-YTD   PIC	9(06)	  COMP-5.
001740	   03  W15-T-YTDV  PIC S9(09)V99  COMP-3.
	   03  W15-V-DESC  PIC	X(15).
       01  W16-DEBTOR-TRAN.
001690	   03  W16-T-DAY   PIC S9(06)	  COMP-3.
001700	   03  W16-T-VAL   PIC S9(09)V99  COMP-3.
001710	   03  W16-T-MTD   PIC S9(06)	  COMP-3.
001720	   03  W16-T-MTDV  PIC S9(09)V99  COMP-3.
001730	   03  W16-T-YTD   PIC S9(06)	  COMP-3.
001740	   03  W16-T-YTDV  PIC S9(09)V99  COMP-3.

       01  W17-DEBTOR-TRAN.
001690	   03  W17-T-DAY   PIC S9(06)	  COMP-3.
001700	   03  W17-T-VAL   PIC S9(09)V99  COMP-3.
001710	   03  W17-T-MTD   PIC S9(06)	  COMP-3.
001720	   03  W17-T-MTDV  PIC S9(09)V99  COMP-3.
001730	   03  W17-T-YTD   PIC S9(06)	  COMP-3.
001740	   03  W17-T-YTDV  PIC S9(09)V99  COMP-3.

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

       01  S-DTRAN.
	   03  LINE  4 COLUMN 31 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "DEBTORS TRANSACTIONS".
	   03  LINE  6 COLUMN  5 VALUE "Transaction Type".
	   03  LINE  8 COLUMN 15 VALUE "Current Amounts".
	   03	       COLUMN 40 VALUE "Adjust by".
	   03	       COLUMN 61 VALUE "New amounts".
	   03  LINE  9 COLUMN 12 VALUE "   No.         Value     No.
      - 	       "     Value     No.         Value".
	   03  LINE 10 COLUMN  5 VALUE "Today".
	   03  LINE 11 COLUMN  5 VALUE "M.T.D.".
	   03  LINE 12 COLUMN  5 VALUE "Y.T.D.".

       01  SD-DTRAN.
	   03  LINE  6 COLUMN 22 FOREGROUND-COLOR 3 HIGHLIGHT
				 PIC X(12) FROM W15-E-DESC.
	   03  LINE 10 COLUMN 12 FOREGROUND-COLOR 3 HIGHLIGHT
				 PIC ZZZZZ9- FROM W15-T-DAY.
	   03	       COLUMN 20 FOREGROUND-COLOR 3 HIGHLIGHT
				 PIC ZZZZZZZZ9.99- FROM W15-T-VAL.
	   03  LINE 11 COLUMN 12 FOREGROUND-COLOR 3 HIGHLIGHT
				 PIC ZZZZZ9- FROM W15-T-MTD.
	   03	       COLUMN 20 FOREGROUND-COLOR 3 HIGHLIGHT
				 PIC ZZZZZZZZ9.99- FROM W15-T-MTDV.
	   03  LINE 12 COLUMN 12 FOREGROUND-COLOR 3 HIGHLIGHT
				 PIC ZZZZZ9- FROM W15-T-YTD.
	   03	       COLUMN 20 FOREGROUND-COLOR 3 HIGHLIGHT
				 PIC ZZZZZZZZ9.99- FROM W15-T-YTDV.

       01  SA-DTRAN.
	   03  LINE 10 COLUMN 34 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5 PIC ZZZZZ9-
				 USING W16-T-DAY AUTO.
	   03	       COLUMN 42 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5 PIC ZZZZZZZZ9.99-
				 USING W16-T-VAL AUTO.
	   03  LINE 11 COLUMN 34 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5 PIC ZZZZZ9-
				 USING W16-T-MTD AUTO.
	   03	       COLUMN 42 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5 PIC ZZZZZZZZ9.99-
				 USING W16-T-MTDV AUTO.
	   03  LINE 12 COLUMN 34 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5 PIC ZZZZZ9-
				 USING W16-T-YTD AUTO.
	   03	       COLUMN 42 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5 PIC ZZZZZZZZ9.99-
				 USING W16-T-YTDV AUTO.

       01  SN-DTRAN.
	   03  LINE 10 COLUMN 56 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 0
				 PIC ZZZZZ9- USING
	   03	       COLUMN 64 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 0
				 PIC ZZZZZZZZ9.99-
	   03  LINE 11 COLUMN 56 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 0
				 PIC ZZZZZ9- USING
	   03	       COLUMN 64 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 0
				 PIC ZZZZZZZZ9.99-
	   03  LINE 12 COLUMN 56 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 0
				 PIC ZZZZZ9- USING
	   03	       COLUMN 64 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 0
				 PIC ZZZZZZZZ9.99-

       01  S-DVAT.
	   03  LINE  4 COLUMN 30 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "DEBTORS VAT STATISTICS".
	   03  LINE  6 COLUMN 16 VALUE "Description".
	   03  LINE  8 COLUMN 23 VALUE "Current Value".
	   03	       COLUMN 40 VALUE "Adjust by".
	   03	       COLUMN 54 VALUE "New Value".
	   03  LINE 10 COLUMN 16 VALUE "Today".
	   03  LINE 11 COLUMN 16 VALUE "M.T.D.".
	   03  LINE 12 COLUMN 16 VALUE "Y.T.D.".

       01  SD-DVAT.
	   03  LINE  6 COLUMN 22 FOREGROUND-COLOR 3 HIGHLIGHT
				 PIC X(15) FROM W15-V-DESC.
	   03  LINE 10 COLUMN 23 FOREGROUND-COLOR 3 HIGHLIGHT
				 PIC ZZZZZZZZ9.99- FROM W15-T-VAL.
	   03  LINE 11 COLUMN 23 FOREGROUND-COLOR 3 HIGHLIGHT
				 PIC ZZZZZZZZ9.99- FROM W15-T-MTDV.
	   03  LINE 12 COLUMN 23 FOREGROUND-COLOR 3 HIGHLIGHT
				 PIC ZZZZZZZZ9.99- FROM W15-T-YTDV.

       01  SA-DVAT.
	   03  LINE 10 COLUMN 23 FOREGROUND-COLOR 3 HIGHLIGHT
				 PIC ZZZZZZZZ9.99- FROM W16-T-VAL.
	   03  LINE 11 COLUMN 23 FOREGROUND-COLOR 3 HIGHLIGHT
				 PIC ZZZZZZZZ9.99- FROM W16-T-MTDV.
	   03  LINE 12 COLUMN 23 FOREGROUND-COLOR 3 HIGHLIGHT
				 PIC ZZZZZZZZ9.99- FROM W16-T-YTDV.

       01  SN-DVAT.
	   03  LINE 10 COLUMN 23 FOREGROUND-COLOR 3 HIGHLIGHT
				 PIC ZZZZZZZZ9.99- FROM W17-T-VAL.
	   03  LINE 11 COLUMN 23 FOREGROUND-COLOR 3 HIGHLIGHT
				 PIC ZZZZZZZZ9.99- FROM W17-T-MTDV.
	   03  LINE 12 COLUMN 23 FOREGROUND-COLOR 3 HIGHLIGHT
				 PIC ZZZZZZZZ9.99- FROM W17-T-YTDV.







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
       AA000	       SECTION.
       AA00.
003240	   IF LS0-PALEV < 9
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AY005.
006590	     PERFORM ZA00-INIT.
	     PERFORM BA000.



       COPY LOCKED.REC.

       COPY FUNCTION.CRT.

       COPY OPTION.CRT.



      /
007310 AN00-READ               SECTION.

       COPY CONTROL.RD.

       COPY DEPART.RD.

       COPY PARAM.RD.

      /
007770 AO00-WRITE              SECTION.

       COPY CONTROL.WR.

       COPY DEPART.WR.

       COPY PARAM.WR.

      /
015340 BA00R	       SECTION 50.
015350 BA00.
029040	     DISPLAY CLR-SCREEN.
	     DISPLAY "AMEND STATISTICS" AT 0233
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
015490	     MOVE 1		 TO WS-NETKEY.
015500       PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
             MOVE 11             TO WS-PARKEY.

       BA05.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE 1		 TO WS-S1.

       BA10.
           IF PAR-E-DESC (WS-S1) = SPACES
	       GO TO BA20.
015700       MOVE PAR-E-DESC (WS-S1) 
				 TO W15-E-DESC.
015710       MOVE PAR-T-DAY (WS-S1)
				 TO W15-T-DAY W17-T-DAY.
015720       MOVE PAR-T-VAL (WS-S1)
				 TO W15-T-VAL W17-T-VAL.
015730       MOVE PAR-T-MTD (WS-S1)
				 TO W15-T-MTD W17-T-MTD.
015740       MOVE PAR-T-MTDV (WS-S1)
				 TO W15-T-MTDV W17-T-MTDV.
015750       MOVE PAR-T-YTD (WS-S1)
				 TO W15-T-YTD W17-T-YTD.
015760       MOVE PAR-T-YTDV (WS-S1)
				 TO W15-T-YTDV W17-T-YTDV.
	     MOVE ZERO		 TO W16-T-DAY
				    W16-T-VAL
				    W16-T-MTD
				    W16-T-MTDV
				    W16-T-YTD
				    W16-T-YTDV.
	     DISPLAY SD-DTRAN.
	     DISPLAY SN-DTRAN.

       BA15.
	     ACCEPT SA-DTRAN.
	     ADD W15-T-DAY   W16-T-DAY	GIVING W17-T-DAY.
	     ADD W15-T-VAL   W16-T-VAL	GIVING W17-T-VAL.
	     ADD W15-T-MTD   W16-T-MTD	GIVING W17-T-MTD.
	     ADD W15-T-MTDV  W16-T-MTDV GIVING W17-T-MTDV.
	     ADD W15-T-YTD   W16-T-YTD	GIVING W17-T-YTD.
	     ADD W15-T-YTDV  W16-T-YTDV GIVING W17-T-YTDV.
	     DISPLAY SN-DTRAN.
	     PERFORM CHECK-CORRECT.
018520	   IF WS-OPTION = "N"
	       GO TO BA15.
015710	     MOVE W17-T-DAY	 TO PAR-T-DAY (WS-S1).
015720	     MOVE W75-T-VAL	 TO PAR-T-VAL (WS-S1).
015730	     MOVE W17-T-MTD	 TO PAR-T-MTD (WS-S1).
015740	     MOVE W17-T-MTDV	 TO PAR-T-MTDV (WS-S1).
015750	     MOVE W17-T-YTD	 TO PAR-T-YTD (WS-S1).
015760	     MOVE W17-T-YTDV	 TO PAR-T-YTDV (WS-S1).

       BA20.
           IF WS-S1 < 2
               ADD 1             TO WS-S1
	       GO TO BA10.
	     PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
           IF WS-PARKEY < 60
               ADD 1             TO WS-PARKEY
	       GO TO BA05.


029040	     DISPLAY CLR-SCREEN.
	     DISPLAY "AMEND STATISTICS" AT 0233
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
             MOVE 9              TO WS-PARKEY.
016350       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE 300            TO WS-DEPKEY.
             PERFORM READ-DEPART THRU READ-DEPART-EXIT.
	     MOVE "VAT SALES"	 TO W15-V-DESC.
	     MOVE PAR-DVAT-DAY	 TO W15-T-VAL W17-T-VAL.
	     MOVE PAR-DVAT-MTD	 TO W15-T-MTDV W17-T-MTDV.
	     MOVE PAR-DVAT-YTD	 TO W15-T-YTDV W17-T-YTDV.
	     PERFORM BA13.
	     MOVE "VAT" 	 TO W15-V-DESC.
	     MOVE DPT-SALES-DAY  TO W15-T-VAL W17-T-VAL.
016380	     MOVE DPT-SALES-MTD  TO W15-T-MTDV W17-T-MTDV.
016390	     MOVE DPT-SALES-YTD  TO W15-T-YTDV W17-T-YTDV.
	     PERFORM BA13.
016410       MOVE "NON VAT SALES"
				 TO W15-V-DESC.
016420	     MOVE PAR-DNGS-DAY	 TO W15-T-VAL W17-T-VAL.
016430	     MOVE PAR-DNGS-MTD	 TO W15-T-MTDV W17-T-MTDV.
016440	     MOVE PAR-DNGS-YTD	 TO W15-T-YTDV W17-T-YTDV.
	     PERFORM BA13.
016460	     MOVE "VAT RETURNS"  TO W15-V-DESC.
016470	     MOVE PAR-CVAT-DAY	 TO W15-T-VAL W17-T-VAL.
016480	     MOVE PAR-CVAT-MTD	 TO W15-T-MTDV W17-T-MTDV.
016490	     MOVE PAR-CVAT-YTD	 TO W15-T-YTDV W17-T-YTDV.
	     PERFORM BA13.
016520	     MOVE "VAT REVERSED" TO W15-V-DESC.
016530	     MOVE DPT-RET-DAY	 TO W15-T-VAL W17-T-VAL.
016530	     MOVE DPT-RET-MTD	 TO W15-T-MTDV W17-T-MTDV.
016540	     MOVE DPT-RET-YTD	 TO W15-T-YTDV W17-T-YTDV.
	     PERFORM BA13.

016560	     MOVE "NON VAT RETURNS"
				 TO W15-V-DESC.
016570	     MOVE PAR-CNGS-DAY	 TO W15-T-VAL W17-T-VAL.
016580	     MOVE PAR-CNGS-MTD	 TO W15-T-MTDV W17-T-MTDV.
016590	     MOVE PAR-CNGS-YTD	 TO W15-T-YTDV W17-T-YTDV.







016620	     MOVE "ACCOUNT RECORDS"
                                 TO R-H26.
	     PERFORM BA13.
016650       MOVE "DEBTOR ACCOUNTS" 
                                 TO R-H36.
016660       MOVE DEB-ACCOUNTS   TO R-N2.
	     PERFORM BA13 3 TIMES.
016690       MOVE "THIS MONTH"   TO R-H26.
	     PERFORM BA13.
016720       MOVE "OPENING BALANCE" 
                                 TO R-H36.
016730       MOVE DEB-OPEN       TO R-V6.
	     PERFORM BA13.
016750       MOVE "DEBITS"       TO R-H36.
016760       MOVE DEB-DT         TO R-V6.
	     PERFORM BA13.
016780       MOVE "CREDITS"      TO R-H36.
016790       MOVE DEB-CR         TO R-V6.
	     PERFORM BA13.
016810       MOVE "CLOSING BALANCE" 
                                 TO R-H36.
016820       MOVE DEB-OUT        TO R-V6.
	     PERFORM BA13 3 TIMES.
016840       MOVE SPACES         TO R-DETAIL.
016850       MOVE "AGE ANALYSIS" TO R-H26.
	     PERFORM BA13.
016880       MOVE DEB-ONE        TO W25-TOTAL.
016890       ADD DEB-TWO         TO W25-TOTAL.
016900       ADD DEB-THREE       TO W25-TOTAL.
016910       ADD DEB-INTEREST    TO W25-TOTAL.
016920	     SUBTRACT W25-TOTAL FROM DEB-OUT
				 GIVING R-V6.
016930       MOVE "CURRENT"      TO R-H36.
	     PERFORM BA13.
016950       MOVE "30 DAYS"      TO R-H36.
016960       MOVE DEB-ONE        TO R-V6.
	     PERFORM BA13.
016980       MOVE "60 DAYS"      TO R-H36.
016990       MOVE DEB-TWO        TO R-V6.
	     PERFORM BA13.
017010       MOVE "90 DAYS"      TO R-H36.
017020       MOVE DEB-THREE      TO R-V6.
	     PERFORM BA13.
017040       MOVE "INTEREST"     TO R-H36.
017050       MOVE DEB-INTEREST   TO R-V6.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
017070	     MOVE SPACES	 TO R-DETAIL.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO R-L1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE "P"		 TO WS-COMMAND
014990	       GO TO BA999.
017080	   IF LS0-STP NOT = 2
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL.
017160	     DISPLAY (9, 35) "Printed" WITH FOREGROUND-COLOR 14.




      /
      *    ****    INITIALIZE ROUTINE    ****
      *
029020 ZA00-INIT              SECTION 89.
       ZA00.
041220	     MOVE 1		 TO WS-S1.
021870	     MOVE SPACES	 TO WS-MID-LNE.
041240 ZA02.
041250	     MOVE WS-G1 	 TO WS-TCHR (WS-S1) WS-BCHR (WS-S1).
	     MOVE WS-G8 	 TO WS-TCH  (WS-S1) WS-BCH  (WS-S1).
041260     IF WS-S1 < 80
041270	       ADD 1		 TO WS-S1
041280         GO TO ZA02.
	     MOVE WS-G9 	 TO WS-TCH  (1).
	     MOVE WS-G10	 TO WS-TCH  (80).
	     MOVE WS-G11	 TO WS-BCH  (1).
	     MOVE WS-G12	 TO WS-BCH  (80).
	     MOVE WS-G14	 TO WS-TCHR (1)  WS-BCHR (1).
	     MOVE WS-G13	 TO WS-TCHR (80) WS-BCHR (80).
021930	     MOVE WS-G2 	 TO WS-TCHR (16) WS-TCHR (41)
021940				    WS-TCHR (64) WS-TCHR (71).
021950	     MOVE WS-G3 	 TO WS-MCHR (16) WS-MCHR (41)
021960				    WS-MCHR (64) WS-MCHR (71)
				    WS-MCHR (1)  WS-MCHR (80).
021970	     MOVE WS-G4 	 TO WS-BCHR (16) WS-BCHR (41)
021980				    WS-BCHR (64) WS-BCHR (71).
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-PTH15
	       MOVE "SupervisorÄ"
				 TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"
				 TO WS-WRKHD
	       MOVE LS-USER	 TO WS-WRKST.
029030 ZA00-OPEN.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
029040	     DISPLAY CLR-SCREEN.
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

       ZA00A.
029110	     MOVE 3		 TO WS-PARKEY.
029120       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
042110	     MOVE WS-PTH15	 TO W02-PTH2 W02-PTH7.
	     MOVE 1		 TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
029890	     MOVE 1		 TO WS-PARKEY.
029900       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
029910	     MOVE PAR-DMY	 TO W12-TODAY.
029920	     MOVE PAR-YMD	 TO W12-T-YMD.
029930	     MOVE PAR-COMPANY	 TO W95-COMP.

030280 ZA999.
030290       EXIT.
      /
030740 ZC00-ERROR             SECTION 89.
      
041480 OPEN-ERROR.
041490       DISPLAY CLR-SCREEN.
041500       DISPLAY "Open error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       PERFORM DISPLAY-FILE-NAME.
      
041480 READ-ERROR.
041490       DISPLAY CLR-SCREEN.
041500       DISPLAY "Read error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       PERFORM DISPLAY-FILE-NAME.
      
041520 WRITE-ERROR.
041530       DISPLAY CLR-SCREEN.
041540       DISPLAY "Write error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       PERFORM DISPLAY-FILE-NAME.
      
041560 DISPLAY-FILE-NAME.
041610	   IF WS-F-ERROR = 2
041620	       MOVE W02-NETWORK  TO WS-FILE
041630	       MOVE WS-NETKEY	 TO WS-KEY
041600     ELSE
041810	   IF WS-F-ERROR = 7
041820	       MOVE W02-DEPART	 TO WS-FILE
041830	       MOVE WS-DEPKEY	 TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 15
041860	       MOVE WS-PARID	 TO WS-FILE
041870	       MOVE WS-PARKEY	 TO WS-KEY.
041880     IF WS-STATUS = "10"
041890	       MOVE "End of FILE"
				 TO WS-STAT-MESSAGE
041900     ELSE
041910     IF WS-STATUS = "22"
041920	       MOVE "Duplicate record number"
				 TO WS-STAT-MESSAGE
041930     ELSE
041940     IF WS-STATUS = "23"
041950	       MOVE "Invalid record number"
				 TO WS-STAT-MESSAGE
041960     ELSE
041970     IF WS-STATUS = "24"
041980	       MOVE "DISK full"  TO WS-STAT-MESSAGE
041990     ELSE
042000     IF WS-STATUS = "30"
042010         MOVE "DISK error" TO WS-STAT-MESSAGE
042020     ELSE
042030     IF WS-STATUS = "94"
042040	       MOVE "FILE locked"
				 TO WS-STAT-MESSAGE.
045380       DISPLAY "File - " AT 1012 WS-FILE WITH FOREGROUND-COLOR 11.
	   IF WS-STAT1 = 9
045390	       DISPLAY "Status " AT 1212
			WS-STAT1 WITH FOREGROUND-COLOR 11
			WS-STAT2 WITH FOREGROUND-COLOR 11
	   ELSE
045390	       DISPLAY "Status " AT 1212
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
