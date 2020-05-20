      /    *************************************************************
      *    **  P U R C H A S E	 S T A T I S T I C S   M T D   O R
      *        Y T D   I N   D E C E N D I N G	 V A L U E   O R
      *        G . P .	 V A L U E   S E Q U E N C E
      *    *************************************************************
021000 BJ000		       SECTION 59.
021010 BJ00.
021020	     DISPLAY CLR-SCREEN.
	     DISPLAY S-PROG4.

       BJ05.
	     PERFORM SAVE-SCREEN-3.
	     MOVE 5		 TO SHADE-ROW.
	     MOVE 27		 TO SHADE-COL.
	     MOVE 30		 TO SHADE-WIDTH.
	     MOVE 4		 TO SHADE-LINES.
	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" AT 0425
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6.
	     DISPLAY "³    Purchases MTD and YTD.    ³" AT 0525
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6.
	     DISPLAY "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´" AT 0625
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6.
	     DISPLAY "³ Starting Month:              ³" AT 0725
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6.
	     DISPLAY "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" AT 0825
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6.
	     PERFORM SCREEN-SHADOW.
	     MOVE W12-MONTH	 TO W30-START W30-END.
	   IF W30-START = 12
	       MOVE 1		 TO W30-START
	   ELSE
	       ADD 1		 TO W30-START.
	     DISPLAY "This routine extracts details from the starting mo
      -		     "nth up to and" AT 1108
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "including the current date. The details are accumu
      -		     "lated for the" AT 1208
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "current month as MTD and for the period as YTD.
      - 	     "             " AT 1308
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.

       BJ10.
	     DISPLAY "Press " AT 2552
		      WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3
		     "Esc" WITH FOREGROUND-COLOR 6 HIGHLIGHT
				BACKGROUND-COLOR 3
		     "ape to Exit"
		      WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3.
	     ACCEPT S05B.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BJ999
		 WHEN OTHER	 CALL X"E5"
		     GO TO BJ10
	       END-EVALUATE.
	   IF W30-START < 01 OR > 12
	       MOVE "Month must be 01 to 12"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
019000	       GO TO BJ10.
	     DISPLAY W30-MONTH(W30-START) AT 0747
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     PERFORM CHECK-CORRECT.
020250	   IF WS-OPTION = "N"
	       GO TO BJ10.
	     DISPLAY CLEAR-L25.
	     MOVE W12-TODAY	 TO W15-START W15-END.
	     MOVE W30-START	 TO W15-SMNTH.
	     MOVE 01		 TO W15-SDAY.
      *    *************************************************************
      *    ****   I S	S T A R T   M O N T H	-   PREVIOUS YEAR
      *    *************************************************************
	   IF W30-END < W30-START
	       SUBTRACT 1	 FROM W15-SCCYY.
      *    *************************************************************
      *    ****    SWITCH START DATE TO YEAR/MONTH/DAY
      *    *************************************************************
	     MOVE W15-SDAY	 TO W15-SDD.
	     MOVE W15-SMNTH	 TO W15-SMM.
	     MOVE W15-SCENT	 TO W15-SCC.
	     MOVE W15-SYEAR	 TO W15-SYY.
      *    *************************************************************
      *    ****    SWITCH END DATE TO YEAR/MONTH/DAY
      *    *************************************************************
	     MOVE W15-EDAY	 TO W15-EDD.
	     MOVE W15-EMNTH	 TO W15-EMM.
	     MOVE W15-ECENT	 TO W15-ECC.
	     MOVE W15-EYEAR	 TO W15-EYY.
	     MOVE "    !"	 TO DBP-AC.
	     PERFORM READ-DEBPRF THRU READ-DEBPRF-EXIT.
      *    *************************************************************
      *    ****    H A V E   T H E S E	 D E T A I L S	 B E E N
      * 	   E X T R A C T E D
      *    *************************************************************
	   IF (DBP-START = W15-S-YMD) AND
	      (DBP-END = W15-E-YMD)
	       GO TO BJ35.
      *    *************************************************************
      *    ****    C R E A T E	 H E A D E R   R E C O R D   O N
      * 	   E X T R A C T   F I L E
      *    *************************************************************
       BJ15.
	     PERFORM RESTORE-SCREEN-3.
	     CLOSE DEBPRF.
	     OPEN OUTPUT DEBPRF.
	     CLOSE DEBPRF.
	     OPEN I-O DEBPRF.
	     INITIALIZE DBP-REC1
	     MOVE "    !"	 TO DBP-AC.
	     MOVE W15-S-YMD	 TO DBP-START.
	     MOVE W15-E-YMD	 TO DBP-END.
	     PERFORM WRITE-DEBPRF THRU WRITE-DEBPRF-EXIT.
      *    *************************************************************
      *    ****    G E T   T H E   F I R S T   D E B T O R
      * 	   A C C O U N T
      *    *************************************************************
	     MOVE " !"		 TO DEB-ACNO.
	     PERFORM START-AT-DEBT-AC THRU READ-DEBTOR-EXIT.
010780	   IF WS-F-ERROR = 6
	       MOVE "No Details" TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BJ999.
             DISPLAY "Debtor account:" AT 1412.

       BJ20.
	     DISPLAY WS-BLNK78 AT 1602 WITH FOREGROUND-COLOR 3.
	     DISPLAY "Extracting:" AT 1612.
015570	     PERFORM READ-DEBTOR-NEXT THRU READ-DEBTOR-EXIT.
010780     IF WS-F-ERROR = 6
015530	       GO TO BJ38.
	     DISPLAY DEB-ACNO AT 1428
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     MOVE DEB-ACNO	 TO CRD-AC.
	     MOVE W15-S-YMD	 TO CRD-DTE.
	     PERFORM START-AT-CRDX-ACNT THRU READ-CARDEX-EXIT.
	   IF WS-F-ERROR = 43
	       GO TO BJ20.
	     MOVE DEB-ACNO	 TO DBP-AC.
	     MOVE DEB-NAME	 TO DBP-NAME.
	     MOVE DEB-SMAN	 TO DBP-SM.
	     MOVE ZERO		 TO DBP-M-GP
				    DBP-M-TCOST
				    DBP-M-TSELL
				    DBP-GP
				    DBP-TCOST
				    DBP-TSELL.

       BJ25.
	     PERFORM READ-CARDEX-NEXT THRU READ-CARDEX-EXIT.
	   IF WS-F-ERROR = 43
	       GO TO BJ30.
	   IF NOT (CRD-AC = DEB-ACNO)
	       GO TO BJ30.
	   IF CRD-DTE > W15-E-YMD
	       GO TO BJ25.
	   IF (CRD-QNT < ZERO)
	       COMPUTE DBP-TCOST = DBP-TCOST +
				 ((CRD-COST * CRD-QNT) * -1)
	       COMPUTE DBP-TSELL = DBP-TSELL +
				 ((CRD-SELL * CRD-QNT) * -1)
	   ELSE
	       COMPUTE DBP-TCOST = DBP-TCOST +
				  (CRD-COST * CRD-QNT)
	       COMPUTE DBP-TSELL = DBP-TSELL +
				  (CRD-SELL * CRD-QNT).
	     MOVE CRD-DTE	 TO W21-DATE.
	   IF W21-MM = W15-EMM
	       IF (CRD-QNT < ZERO)
		   COMPUTE DBP-M-TCOST = DBP-M-TCOST +
				       ((CRD-COST * CRD-QNT) * -1)
		   COMPUTE DBP-M-TSELL = DBP-M-TSELL +
				       ((CRD-SELL * CRD-QNT) * -1)
	       ELSE
		   COMPUTE DBP-M-TCOST = DBP-M-TCOST +
					(CRD-COST * CRD-QNT)
		   COMPUTE DBP-M-TSELL = DBP-M-TSELL +
					(CRD-SELL * CRD-QNT).
	     GO TO BJ25.

       BJ30.
	   IF (DBP-TCOST = ZERO) AND
	      (DBP-TSELL = ZERO)
	       GO TO BJ20.
	     SUBTRACT DBP-M-TCOST FROM DBP-M-TSELL
				 GIVING DBP-M-GP.
	     SUBTRACT DBP-TCOST FROM DBP-TSELL
				 GIVING DBP-GP.
	     PERFORM WRITE-DEBPRF THRU WRITE-DEBPRF-EXIT.
	     GO TO BJ20.

       BJ35.
018110	     MOVE "Details have been extracted - use Y/N?"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
016020	   IF NOT (WS-OPTION = "N" OR "Y")
016030	       GO TO BJ35.
	   IF WS-OPTION = "N"
	       GO TO BJ15.
	     PERFORM RESTORE-SCREEN-3.

       BJ38.
	     MOVE "'D'isplay or 'P'rinter format [D]"
				 TO WS-ERR-MES.
	     MOVE "D"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
020250	   IF NOT (WS-OPTION = "D" OR "P")
	       GO TO BJ38.
	     MOVE WS-OPTION	 TO WS-ENQD.
	     MOVE "Y"		 TO WS-MTD-YTD.
	   IF WS-ENQD = "P"
	       GO TO BJ40.

       BJ40.
	     MOVE 10		 TO SHADE-ROW.
	     MOVE 24		 TO SHADE-COL.
	     MOVE 35		 TO SHADE-WIDTH.
	     MOVE 5		 TO SHADE-LINES.
	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 0922
		      WITH FOREGROUND-COLOR 5 HIGHLIGHT
			   BACKGROUND-COLOR 5
		     "¿" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
	     DISPLAY "³ " AT 1022 WITH FOREGROUND-COLOR 5 HIGHLIGHT
				       BACKGROUND-COLOR 5
		     "R E P O R T I N G	  O P T I O N S"
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 5
		     " ³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
	     DISPLAY "³ " AT 1122 WITH FOREGROUND-COLOR 5 HIGHLIGHT
				       BACKGROUND-COLOR 5
		     "A" WITH FOREGROUND-COLOR 7 HIGHLIGHT
			      BACKGROUND-COLOR 5
		     "ll extracted accounts           "
		      WITH FOREGROUND-COLOR 3 BACKGROUND-COLOR 5
		     " ³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
	     DISPLAY "³ " AT 1222 WITH FOREGROUND-COLOR 5 HIGHLIGHT
				       BACKGROUND-COLOR 5
		     "S" WITH FOREGROUND-COLOR 7 HIGHLIGHT
			      BACKGROUND-COLOR 5
		     "elected - top 10/20/50/100/etc  "
		      WITH FOREGROUND-COLOR 3 BACKGROUND-COLOR 5
		     " ³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
	     DISPLAY "³ " AT 1322 WITH FOREGROUND-COLOR 5 HIGHLIGHT
				       BACKGROUND-COLOR 5
		     "Select option required ("
		      WITH FOREGROUND-COLOR 3 BACKGROUND-COLOR 5
		     "A" WITH FOREGROUND-COLOR 7 HIGHLIGHT
			      BACKGROUND-COLOR 5
		     "/" WITH FOREGROUND-COLOR 3 BACKGROUND-COLOR 5
		     "S" WITH FOREGROUND-COLOR 7 HIGHLIGHT
			      BACKGROUND-COLOR 5
		     ")     " WITH FOREGROUND-COLOR 3 BACKGROUND-COLOR 5
		     " ³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
	     DISPLAY "À" AT 1422 WITH FOREGROUND-COLOR 5 HIGHLIGHT
				      BACKGROUND-COLOR 5
		     "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
	     PERFORM SCREEN-SHADOW.
	     MOVE "S"		 TO WS-OPTION.
	     DISPLAY "Press " AT 2552
		      WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3
		     "Esc" WITH FOREGROUND-COLOR 6 HIGHLIGHT
				BACKGROUND-COLOR 3
		     "ape to Exit"
		      WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3.

       BJ45.
	     ACCEPT WS-OPTION AT 1353
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BJ999
		 WHEN OTHER	 CALL X"E5"
		     GO TO BJ45
	       END-EVALUATE.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-OPTION = "A" OR "S")
	       GO TO BJ45.
	     PERFORM RESTORE-SCREEN-3.
	     MOVE WS-OPTION	 TO WS-VIEW.
	   IF WS-OPTION = "A"
	       GO TO BJ55.
	     MOVE 11		 TO SHADE-ROW.
	     MOVE 26		 TO SHADE-COL.
	     MOVE 31		 TO SHADE-WIDTH.
	     MOVE 2		 TO SHADE-LINES.
	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 1024
		      WITH FOREGROUND-COLOR 5 HIGHLIGHT
			   BACKGROUND-COLOR 5
		     "¿" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
	     DISPLAY "³ " AT 1124 WITH FOREGROUND-COLOR 5 HIGHLIGHT
				       BACKGROUND-COLOR 5
		     "Report/View top      accounts"
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 5
		     " ³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
	     DISPLAY "À" AT 1224 WITH FOREGROUND-COLOR 5 HIGHLIGHT
				      BACKGROUND-COLOR 5
		     "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
	     PERFORM SCREEN-SHADOW.
	     MOVE ZERO		 TO W25-RESULT.
	     DISPLAY "Press " AT 2552
		      WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3
		     "Esc" WITH FOREGROUND-COLOR 6 HIGHLIGHT
				BACKGROUND-COLOR 3
		     "ape to Exit"
		      WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3.

       BJ50.
	     ACCEPT S40.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BJ999
		 WHEN OTHER	 CALL X"E5"
		     GO TO BJ50
	       END-EVALUATE.
	   IF W25-WHOLE = ZERO
	       GO TO BJ50.
	     MOVE W25-WHOLE	 TO WS-S.
	     MOVE ZERO		 TO WS-S1.
	     PERFORM RESTORE-SCREEN-3.

       BJ55.
	     PERFORM BJ125.

       BJ60.
	   IF WS-ENQD = "P"
	       PERFORM OPEN-PRINTER
	       PERFORM BJ155
	   ELSE
	       DISPLAY S-TOP
	       MOVE 0601	 TO CPOS
	       PERFORM BJ100-CRT-EDGE UNTIL CLIN = 22.
	     MOVE 999999999.99	 TO DBP-TSELL.
	     MOVE ZERO		 TO WS-S1.
	   IF WS-ENQ = "A"
	       PERFORM START-AT-DBPR-Y-SELL THRU READ-DEBPRF-EXIT
	   ELSE
	       MOVE WS-SMAN	 TO DBP-SM
	       PERFORM START-AT-DBPR-SM-SELL THRU READ-DEBPRF-EXIT
	       DISPLAY "Sales person: " AT 0302
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
			WS-SMAN WITH FOREGROUND-COLOR 7 HIGHLIGHT
				     BACKGROUND-COLOR 7
		       " " WITH FOREGROUND-COLOR 7 HIGHLIGHT
				BACKGROUND-COLOR 7
			W60-SMAN WITH FOREGROUND-COLOR 7 HIGHLIGHT
				      BACKGROUND-COLOR 7.
	   IF WS-F-ERROR = 56
	       MOVE "No data on file"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BJ999.

       BJ65.
	     MOVE 0601		 TO CPOS.

       BJ70.
	   IF WS-OPTION = "J"
               MOVE SPACE        TO WS-OPTION
	       GO TO BJ75.
      *    *************************************************************
      *    ****    C H E C K   I F   R E Q U E S T E D
      * 	   S E L E C T I O N   H A S   B E E N
      * 	   D I S P L A Y E D
      *    *************************************************************
	   IF NOT (WS-VIEW = "A")
	       IF WS-S1 = WS-S
		   GO TO BJ80.
	     PERFORM READ-DEBPRF-PREV THRU READ-DEBPRF-EXIT.
	   IF WS-ENQ = "S"
	       IF NOT (DBP-SM = WS-SMAN)
		   GO TO BJ145.
	   IF WS-F-ERROR = 56
	       GO TO BJ145.
	   IF DBP-AC = "    !"
	       GO TO BJ70.
	     ADD 1		 TO WS-S1.
	   IF WS-ENQD = "P"
	       GO TO BJ165.

       BJ75.
	     DISPLAY (CLIN, 2) DBP-AC
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY ( , 9) DBP-SNAME
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     MOVE DBP-TSELL	 TO W100-S9V99.
	     DISPLAY ( , 35) W100-S9V99
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     MOVE DBP-TCOST	 TO W100-S9V99.
	     DISPLAY ( , 48) W100-S9V99
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     MOVE DBP-GP	 TO W100-S9V99.
	     DISPLAY ( , 61) W100-S9V99
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     COMPUTE W100-S3V99 = (DBP-GP / DBP-TSELL) * 100.000.
	     DISPLAY ( , 74) W100-S3V99
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
000970	   IF CLIN < 21
000980	       ADD 1		 TO CLIN
000990	       GO TO BJ70.
001000
001010 BJ80.
	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
001020	     DISPLAY "PgUp" AT 2303 WITH FOREGROUND-COLOR 14
                     "/" WITH FOREGROUND-COLOR 7
		     "PgDn" WITH FOREGROUND-COLOR 14
		     " - " WITH FOREGROUND-COLOR 7
001170		     "Esc" WITH FOREGROUND-COLOR 14
		     "ape." WITH FOREGROUND-COLOR 7.

       BJ85.
	     PERFORM SAVE-SCREEN-2.
001140	     MOVE 0		 TO W75-S1.
001150       MOVE 0601           TO CPOS.

001180 BJ90.
	     MOVE 1920		 TO W41-LENGTH.
	     CALL "CBL_WRITE_SCR_ATTRS" USING SAVE-POS
					      W42-ATTRIB2
					      W41-LENGTH
					RETURNING WS-STATUS.
001220	     MOVE 2000		 TO W41-LENGTH.

001240 BJ95.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	     DISPLAY CLEAR-L25.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     PERFORM RESTORE-SCREEN-3
		     GO TO BJ150
		 WHEN PAGE-DOWN
001340		     GO TO BJ105
		 WHEN PAGE-UP
001340		     GO TO BJ115
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BJ95.
	     CALL X"E5".
001280	     GO TO BJ95.

       BJ100-CRT-EDGE.
	     DISPLAY "³" AT CPOS WITH FOREGROUND-COLOR 7 HIGHLIGHT
				      BACKGROUND-COLOR 0
		     "      ³                         ³            ³
      -		     "        ³            ³      "
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7
		     "³" WITH FOREGROUND-COLOR 7 HIGHLIGHT
			      BACKGROUND-COLOR 0.
002090	     ADD 1		 TO CLIN.

       BJ105.
001480	   IF CLIN < 21
001490	       GO TO BJ75.

       BJ110.
001500	     MOVE 0601		 TO CPOS.
	     PERFORM BJ100-CRT-EDGE UNTIL CLIN = 22.
001560	     GO TO BJ65.

       BJ115.
             MOVE 1              TO WS-IXS1.
      *    *************************************************************
      *    ****    SET OPTION TO BYPASS THE READ NEXT
      *    *************************************************************
	      MOVE "J"		  TO WS-OPTION.

       BJ120.
	     PERFORM READ-DEBPRF-NEXT THRU READ-DEBPRF-EXIT.
	    IF WS-F-ERROR = 56
	       MOVE SPACE	 TO WS-OPTION
001500	       MOVE 0602	 TO CPOS
	       GO TO BJ110.
	   IF WS-ENQ = "S"
	       IF NOT (DBP-SM = WS-SMAN)
		   MOVE SPACE	 TO WS-OPTION
001500		   MOVE 0602	 TO CPOS
		   GO TO BJ110.
	    IF WS-IXS1 < 36
		ADD 1		 TO WS-IXS1
		SUBTRACT 1	 FROM WS-S1
		END-IF
		GO TO BJ120.
	     GO TO BJ110.

       BJ125.
	     MOVE "'A'll accounts or per 'S'alesperson [A]"
				 TO WS-ERR-MES.
	     MOVE "A"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
020250	   IF NOT (WS-OPTION = "A" OR "S")
	       GO TO BJ125.
	     MOVE WS-OPTION	 TO WS-ENQ.
	   IF WS-OPTION = "S"
	       PERFORM BJ130 THRU BJ140
	       IF WS-SMAN = ZERO
		   GO TO BJ125.

       BJ130.
	     MOVE 10		 TO SHADE-ROW.
	     MOVE 24		 TO SHADE-COL.
	     MOVE 35		 TO SHADE-WIDTH.
	     MOVE 5		 TO SHADE-LINES.
	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 0922
		      WITH FOREGROUND-COLOR 5 HIGHLIGHT
			   BACKGROUND-COLOR 5
		     "¿" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
	     DISPLAY "³" AT 1022 WITH FOREGROUND-COLOR 5 HIGHLIGHT
					 BACKGROUND-COLOR 5
		     "S E L E C T   S A L E S P E R S O N"
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 5
		     "³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
	     DISPLAY "³ " AT 1122 WITH FOREGROUND-COLOR 5 HIGHLIGHT
					 BACKGROUND-COLOR 5
		     "No Name                           "
		      WITH FOREGROUND-COLOR 7 BACKGROUND-COLOR 5
		     "³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
	     DISPLAY "³                                   " AT 1222
		      WITH FOREGROUND-COLOR 5 HIGHLIGHT
			   BACKGROUND-COLOR 5
		     "³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
	     DISPLAY "À" AT 1322 WITH FOREGROUND-COLOR 5 HIGHLIGHT
				      BACKGROUND-COLOR 5
		     "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
	     MOVE ZERO		 TO WS-SMAN.

       BJ135.
	     ACCEPT S41.
	   IF WS-SMAN > 50
	       GO TO BJ135.
	   IF WS-SMAN = ZERO
	       GO TO BJ140.
009820	     ADD 100 WS-SMAN	 GIVING WS-PARKEY.
009830       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
009840     IF PAR-SALESMAN = SPACES
009850	       GO TO BJ135.
009860	     DISPLAY PAR-SALESMAN AT 1227
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     MOVE PAR-SALESMAN	 TO W60-SMAN.

       BJ140.
	     MOVE 15		 TO SLIN.
	     PERFORM CHECK-SETUP THRU CHECK-EXIT.
015980	   IF WS-OPTION = "N"
	       GO TO BJ135.

       BJ145.
	     DISPLAY "End - Press ANY key" AT 2512
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 6 HIGHLIGHT.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       BJ150.
	     PERFORM RESTORE-SCREEN-3.
	   IF WS-ENQ = "A"
	       GO TO BJ995.
	     MOVE "'N'ext Sales person or 'E'xit [N]"
				 TO WS-ERR-MES.
	     MOVE "N"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
020250	   IF NOT (WS-OPTION = "N" OR "E")
	       GO TO BJ150.
	   IF WS-OPTION = "E"
	       GO TO BJ995.
	   IF WS-OPTION = "N"
	       PERFORM BJ130 THRU BJ140
	       IF WS-SMAN = ZERO
		   GO TO BJ995.
	     PERFORM BJ995.
	     GO TO BJ60.
      *    *************************************************************
      *    ****   R E P O R T	H E A D I N G
      *    *************************************************************
       BJ155.
	     MOVE SPACES	 TO W100-DESC.
	     MOVE W30-MONTH(W30-START)
				 TO W30-START-N.
	     MOVE W30-MONTH(W30-END)
				 TO W30-END-N.
	     STRING W30-START-N DELIMITED BY " "
		    " to " DELIMITED SIZE
		    W30-END-N DELIMITED BY " "
		    INTO W100-DESC.
       BJ155-EXIT.
	     EXIT.
      *    *************************************************************
      *    ****   1 0	C P I
      *    *************************************************************
       BJ160.
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE "1"		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL.
	     MOVE "P"		 TO WS-COMMAND.
	     MOVE "H"		 TO W02-PRN-TYPE.
015350	     MOVE SPACES	 TO REP-DETAIL1.
015360	     MOVE "PAGE:"	 TO PRF-PAGE.
015370	     MOVE "DATE:"	 TO PRF-DATE.
015380	     ADD 1		 TO WS-PAGE.
015390	     MOVE W12-TODAY	 TO PRF-DTE.
015400	     MOVE WS-PAGE	 TO PRF-P-NO.
015440	     MOVE W100-HEAD	 TO PRF-COMPANY.
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO REP-DETAIL1.
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE "E"		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL.
	     MOVE "H"		 TO W02-PRN-TYPE.
015410	     MOVE W95-COMP	 TO REP-CO.
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
015460       MOVE SPACES TO REP-DETAIL1.
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE "X"		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL.
015460	     MOVE SPACES	 TO REP-DETAIL1.
	     MOVE "H"		 TO W02-PRN-TYPE.
	   IF WS-ENQ = "S"
	       MOVE "Sales Person:"
				 TO PRF-SP-H
	       MOVE WS-SMAN	 TO PRF-SP-NO
	       MOVE W60-SMAN	 TO PRF-SP-N
	       IF NOT (WS-PRINTER = "S")
		   MOVE 0	 TO WS-ADVANCE
		   PERFORM CALL-PRINTUTL
	       END-IF
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE SPACES	 TO REP-DETAIL1.
	     MOVE "________________Month-to-date________________"
				 TO PRF-H3.
	     MOVE "________________Year-to-date_________________"
				 TO PRF-H4.
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO REP-DETAIL1.
	     MOVE "Act No"	 TO PRF-H1.
	     MOVE "Name"	 TO PRF-H2.
	     MOVE "Total Sell"	 TO PRF-H3A PRF-H4A.
	     MOVE "Total Cost"	 TO PRF-H3B PRF-H4B.
	     MOVE "GP Value"	 TO PRF-H3C PRF-H4C.
	     MOVE "GP %"	 TO PRF-H3D PRF-H4D.
	     MOVE "Seq" 	 TO PRF-H5.
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO REP-DETAIL1.
	     MOVE "D"		 TO W02-PRN-TYPE.

       BJ165.
	   IF WS-PAGE = ZERO
	       PERFORM BJ160.
	     MOVE DBP-AC	 TO PRF-ACNO.
	     MOVE DBP-NAME	 TO PRF-NAME.
	     MOVE DBP-M-TSELL	 TO PRF-MTD-S.
	     MOVE DBP-M-TCOST	 TO PRF-MTD-C.
	     MOVE DBP-M-GP	 TO PRF-MTD-GP.
	     COMPUTE PRF-MTD-PER = (DBP-M-GP / DBP-M-TSELL) * 100.000.
	     MOVE DBP-TSELL	 TO PRF-YTD-S.
	     MOVE DBP-TCOST	 TO PRF-YTD-C.
	     MOVE DBP-GP	 TO PRF-YTD-GP.
	     COMPUTE PRF-YTD-PER = (DBP-GP / DBP-TSELL) * 100.000.
	     MOVE WS-S1 	 TO PRF-SEQ.
016020	   IF W02-LINAGE < (W02-PRN-LENGTH - 4)
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
016040     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
016060	       PERFORM BJ160.
016070	     MOVE SPACES	 TO REP-DETAIL1.
	     PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO REP-DETAIL1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
014990	       GO TO BJ995.

       BJ170.
	     PERFORM READ-DEBPRF-PREV THRU READ-DEBPRF-EXIT.
	   IF WS-ENQ = "S"
	       IF NOT (DBP-SM = WS-SMAN)
		   GO TO BJ175.
	   IF WS-F-ERROR = 56
	       GO TO BJ175.
	   IF DBP-AC = "    !"
	       GO TO BJ170.
	     ADD 1		 TO WS-S1.
	     GO TO BJ165.

       BJ175.
	     MOVE "E"		 TO WS-COMMAND.
	     MOVE 99		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     PERFORM RESTORE-SCREEN-3.
	     GO TO BJ145.

       BJ995.
	   IF WS-ENQD = "P"
	       MOVE "C"		 TO WS-COMMAND
	       PERFORM CALL-PRINTUTL.
	     MOVE ZERO		 TO WS-PAGE.

       BJ999.
		    EXIT.
