      $SET LINKCOUNT"256"
      ******************************************************************
      *                                                                *
      *   *******   *******   *******	  ****	  ********    ******   *
      *   **	**  **	  **  **    **	 **  **   **	     **    **  *
      *   **	**  **	  **  **    **	**    **  *******    **        *
      *   *******   ******    *******	**    **	**   *******   *
      *   **	    **	 **   **	**    **	**   **    **  *
      *   **	    **	  **  **	 **  **   **	**   **    **  *
      *   **	    **	  **  **	  ****	   ******     ******   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *    P A R A M E T E R   S P E C I A L   R O U T I N E S	       *
      *                                                                *
      *     VERSION 8.06 - August 2002				       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     PRP056.
000030 AUTHOR.         J.W.LEMMON (APAC).
000040 DATE-WRITTEN.   JANUARY 1983.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1983 - 2002
				     by James William Lemmon.

		   All rights reserved.

000050 SECURITY.   PROGRAM MATERIAL IS THE SOLE PROPERTY OF
000060		   James William Lemmon.(Id No. 4412165050082).

000050 ENVIRONMENT DIVISION.
000060 CONFIGURATION SECTION.
000070 SPECIAL-NAMES.  
000080		       CURSOR IS CSTART
000090		       CONSOLE IS CRT
		       CRT STATUS IS KEY-STATUS.
000090 INPUT-OUTPUT SECTION.
000100 FILE-CONTROL.

       COPY APACFIDS.SL.

000140 COPY PARAM.SL.

000170 DATA DIVISION.
000180 FILE SECTION.

       COPY APACFIDS.FDE.

000220 COPY PARAM.FDE.

      /
      *         **         **    ******    *******    **    **
      *         **         **   **    **   **    **   **   ** 
      *         **    *    **   **    **   **   **    **  **  
      *         **   ***   **   **    **   ******     *****   
      *          ** ** ** **    **    **   **   **    **  **  
      *           ***   ***     **    **   **    **   **   ** 
      *            *     *       ******    **    **   **    **
      *
000940 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
000950 77  WS-S1	   PIC	9(04)	 COMP-5.
000960 77  WS-S2	   PIC	9(04)	 COMP-5.
000970 77  WS-S3	   PIC	9(04)	 COMP-5.
000980 77  WS-S4	   PIC	9(04)	 COMP-5.
000990 77  WS-S5	   PIC	9(04)	 COMP-5.
001000 77  WS-S6	   PIC	9(04)	 COMP-5.
001000 77  WS-S7	   PIC	9(04)	 COMP-5.
001000 77  WS-S8	   PIC	9(04)	 COMP-5.
001010 77  WS-PARKEY	   PIC	9(04)	 COMP-5.
001010 77  WS-NETKEY	   PIC	9(04)	 COMP-5.
001020 77  WS-SECKEY	   PIC	9(04)	 COMP-5.
001070 77  WS-NUM	   PIC	Z9.
001070 77  WS-SNO	   PIC	9(02).
001100 77  WS-OPTION	   PIC	X(01).
001120 77  WS-ERROR	   PIC	9(01).
       77  WS-OVER-LIM	   PIC	X(01).
       77  WS-ARREARS	   PIC	X(01).
       77  WS-PASS	   PIC	X(06).
001160 77  WS-COMP	   PIC	9(01).
001170 77  WS-USE-GROUP    PIC	X(01).
001170 77  WS-TYPE	   PIC	X(01).
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
002430	   03  WS-TOP-LNE2.
002440	       05  WS-TCH  PIC	X(80) VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
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
002450     03  WS-MID-LNE2.
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

001260 COPY WS.WS.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002040 01  W02-FID.

       COPY APACFIDS.ID.

       COPY PARAM.ID.

       COPY W12.WS.

       01  W15-CONTROL.
           03  W15-DEBINV     PIC  9(08)    COMP-3.
           03  W15-DEBREC     PIC  9(08)    COMP-3.
           03  W15-DEBORD     PIC  9(08)    COMP-3.
           03  W15-DEBJOB     PIC  9(08)    COMP-3.
           03  W15-CRDORD     PIC  9(08)    COMP-3.
001490 01  W15-HEX.
001500     03  W15-ASCII       PIC X(01) OCCURS 256.

       COPY W20.WS.

001890 01  W25-CALCS.
001900     03  W25-RESULT     PIC 9(07)V99.
001910     03  W25-RESULT1 REDEFINES W25-RESULT.
001920         05  FILLER      PIC 9(01).
001930         05  W25-WHOLE  PIC 9(06).
001940         05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY    PIC 9(06).
               05  W25-SUB    PIC 9(01).
               05  FILLER     PIC 9(02).
001950     03  W25-PASS.
001960         05  W25-PASS1  PIC 9(03).
001970         05  W25-PASS2  PIC 9(03).
001980     03  W25-TIME       PIC 9(08).
001990     03  W25-TRED REDEFINES W25-TIME.
002000         05  FILLER      PIC 9(02).
002010         05  W25-FACT   PIC 9(06).
002020     03  W25-TOTAL      PIC S9(07)V99  COMP-3.
002030     03  W25-VALUE      PIC S9(07)V99  COMP-3.

       COPY W40.WS.

       COPY FUNCTION.WS.

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
      *   ******    ******   *******   ********  ********  **     ** 
      *  **    **  **    **  **    **  **        **        ***    ** 
      *  **        **        **    **  **        **        ** *   ** 
      *   ******   **        ******    *****     *****     **  *  ** 
      *        **  **        **   **   **        **        **   * ** 
      *  **    **  **    **  **    **  **        **        **    *** 
      *   ******    ******   **    **  ********  ********  **     ** 
      *                                                              
003150 SCREEN SECTION.

       COPY BLANK.CRT.

      * 01  S34.
      *	   03  LINE 22 COLUMN 51 FOREGROUND-COLOR 7 HIGHLIGHT
      *				 BACKGROUND-COLOR 4
      *				 PIC Z9 USING PAR-SLNGTH AUTO.

       01  S35.
	   03  LINE 17 COLUMN 51 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4
				 PIC Z(06)9 USING PAR-INV-AMT AUTO.

       01  S36.
	   03  LINE 11 COLUMN 49 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ9.999 USING PAR-HFEE AUTO.

       COPY S99.CRT.

       COPY ERROR.CRT.

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
006220 AA000-MAIN	       SECTION.
006230 AA000-INIT.
003240	   IF LS0-PALEV < 9
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
006250	     PERFORM ZA000-INIT.
	     PERFORM YA000.
       AA49.
             EXIT PROGRAM.

       COPY LOCKED.RC2.

       COPY FUNCTION.CRT.

       AA200              SECTION.
       AA201.
002130	     DISPLAY "³" AT CPOS WITH BACKGROUND-COLOR 0
				      FOREGROUND-COLOR 3
		     WS-BLNK78 WITH FOREGROUND-COLOR 3
		     "³" WITH BACKGROUND-COLOR 0
			      FOREGROUND-COLOR 3.
019260	     ADD 1		 TO CLIN.
       AA205.
             EXIT.
      /
      *    ****    READ FILES ROUTINES
      *
006630 AC000-READ              SECTION.

       COPY PARAM.RD.

      *
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
006940 AD000-WRITE             SECTION.

       COPY PARAM.WR.

      /
      *    *****    S U N D R Y	  P A R A M E T E R S	*****
      *
008640 YA000		       SECTION 50.
013640 YA340-COST.
013650       DISPLAY CLR-SCREEN.
	     PERFORM SAVE-SCREEN-3.
	     MOVE 5		 TO SHADE-ROW.
	     MOVE 24		 TO SHADE-COL.
	     MOVE 36		 TO SHADE-WIDTH.
	     MOVE 17		 TO SHADE-LINES.
005660	     DISPLAY "SPECIAL ROUTINES" AT 0233
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" AT 0322
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6.
	     DISPLAY "³Log sales to CARDEX file            ³" AT 0422
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6.
	     DISPLAY "³Update priced item QUANTITIES       ³" AT 0522
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6.
	     DISPLAY "³Use bank deposit rountine           ³" AT 0622
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6.
	     DISPLAY "³Use Prompts with Sales   " AT 0722
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6
		     "Y" WITH FOREGROUND-COLOR 2 HIGHLIGHT
			      BACKGROUND-COLOR 6
		     "es/" WITH FOREGROUND-COLOR 6 HIGHLIGHT
				BACKGROUND-COLOR 6
		     "N" WITH FOREGROUND-COLOR 2 HIGHLIGHT
			      BACKGROUND-COLOR 6
		     "o/" WITH FOREGROUND-COLOR 6 HIGHLIGHT
			       BACKGROUND-COLOR 6
		     "F" WITH FOREGROUND-COLOR 2 HIGHLIGHT
			      BACKGROUND-COLOR 6
		     "ast³" WITH FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6.
	     DISPLAY "³DEFAULT: PLU's or Stock codes       ³" AT 0822
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6.
	     DISPLAY "³Use Prompts with Purchases          ³" AT 0922
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6.
	     DISPLAY "³Pint Cash Slips (Default)           ³" AT 1022
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6.
	     DISPLAY "³Use Item/Xref code for BAR CODES    ³" AT 1122
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6.
	     DISPLAY "³Bar Code Height (Dots)     (50-256) ³" AT 1222
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6.
	     DISPLAY "³Debtors - Use group codes           ³" AT 1322
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6.
	     DISPLAY "³Invoicing - Ignore Limit            ³" AT 1422
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6.
	     DISPLAY "³Invoicing - Ignore Arrears          ³" AT 1522
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6.
	     DISPLAY "³Invoice -Check High Value           ³" AT 1622
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6.
	     DISPLAY "³Use 3 decimals - Purch. Quantity    ³" AT 1722
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6.
	     DISPLAY "³Allow for No. of Parcels (Inv.)     ³" AT 1822
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6.
	     DISPLAY "³Use split Bar code (Code/Quant)     ³" AT 1922
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6.
	     DISPLAY "³Use handling fee (Stock Trnfer)     ³" AT 2022
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6.
	     DISPLAY "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" AT 2122
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6.
	     PERFORM SCREEN-SHADOW.
	     MOVE 5		 TO WS-PARKEY.
013680	     PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE PAR-USE-GROUP  TO WS-USE-GROUP.
013670	     MOVE 3		 TO WS-PARKEY.
013680       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	   IF NOT (PAR-OVER-LIM = "N" OR "Y")
	       MOVE "N" 	 TO PAR-OVER-LIM.
	   IF NOT (PAR-ARREARS = "N" OR "Y")
	       MOVE "N" 	 TO PAR-ARREARS.
	   IF NOT (PAR-INV-CHECK = "N" OR "Y")
	       MOVE "N" 	 TO PAR-INV-CHECK
	       MOVE ZERO	 TO PAR-INV-AMT.
	     MOVE PAR-OVER-LIM	 TO WS-OVER-LIM.
	     MOVE PAR-ARREARS	 TO WS-ARREARS.
	   IF NOT (PAR-USE-3-DEC = "N" OR "Y")
	       MOVE "N" 	 TO PAR-USE-3-DEC.
	   IF NOT (PAR-PARCELS = "N" OR "Y")
	       MOVE "N" 	 TO PAR-PARCELS.
	   IF NOT (PAR-SPLIT-BC = "N" OR "Y")
	       MOVE "N" 	 TO PAR-SPLIT-BC.
	   IF NOT (PAR-HND-FEE = "N" OR "Y")
	       MOVE "N" 	 TO PAR-HND-FEE
	       MOVE ZERO	 TO PAR-HFEE.
	   IF NOT (WS-USE-GROUP = "N" OR "Y")
	       MOVE "N" 	 TO WS-USE-GROUP.

013890 YA345.
013900	     ACCEPT PAR-CRDX AT 0448
013910		    WITH BACKGROUND-COLOR 4
			 FOREGROUND-COLOR 7 HIGHLIGHT
			 UPDATE AUTO.
013900	     ACCEPT PAR-PRICED-IND AT 0553
013910		    WITH BACKGROUND-COLOR 4
			 FOREGROUND-COLOR 7 HIGHLIGHT
			 UPDATE AUTO.
013900	     ACCEPT PAR-BANK-IND AT 0649
013910		    WITH BACKGROUND-COLOR 4
			 FOREGROUND-COLOR 7 HIGHLIGHT
			 UPDATE AUTO.
	     ACCEPT PAR-PROMPT AT 0746
013910		    WITH BACKGROUND-COLOR 4
			 FOREGROUND-COLOR 7 HIGHLIGHT
			 UPDATE AUTO.
	     ACCEPT PAR-PLU-D AT 0853
013910		    WITH BACKGROUND-COLOR 4
			 FOREGROUND-COLOR 7 HIGHLIGHT
			 UPDATE AUTO.
	     ACCEPT PAR-PPROMPT AT 0950
013910		    WITH BACKGROUND-COLOR 4
			 FOREGROUND-COLOR 7 HIGHLIGHT
			 UPDATE AUTO.
	     ACCEPT PAR-SLIP-P AT 1049
013910		    WITH BACKGROUND-COLOR 4
			 FOREGROUND-COLOR 7 HIGHLIGHT
			 UPDATE AUTO.
	     ACCEPT PAR-BAR-CODE AT 1156
013910		    WITH BACKGROUND-COLOR 4
			 FOREGROUND-COLOR 7 HIGHLIGHT
			 UPDATE AUTO.
	     ACCEPT PAR-BCSIZE AT 1246
013910		    WITH BACKGROUND-COLOR 4
			 FOREGROUND-COLOR 7 HIGHLIGHT
			 UPDATE AUTO.
	     ACCEPT WS-USE-GROUP AT 1349
013910		    WITH BACKGROUND-COLOR 4
			 FOREGROUND-COLOR 7 HIGHLIGHT
			 UPDATE AUTO.
	     ACCEPT PAR-OVER-LIM AT 1448
013910		    WITH BACKGROUND-COLOR 4
			 FOREGROUND-COLOR 7 HIGHLIGHT
			 UPDATE AUTO.
	     ACCEPT PAR-ARREARS AT 1550
013910		    WITH BACKGROUND-COLOR 4
			 FOREGROUND-COLOR 7 HIGHLIGHT
			 UPDATE AUTO.
	     ACCEPT PAR-INV-CHECK AT 1649
013910		    WITH BACKGROUND-COLOR 4
			 FOREGROUND-COLOR 7 HIGHLIGHT
			 UPDATE AUTO.
	     ACCEPT PAR-USE-3-DEC AT 1756
013910		    WITH BACKGROUND-COLOR 4
			 FOREGROUND-COLOR 7 HIGHLIGHT
			 UPDATE AUTO.
	     ACCEPT PAR-PARCELS AT 1855
013910		    WITH BACKGROUND-COLOR 4
			 FOREGROUND-COLOR 7 HIGHLIGHT
			 UPDATE AUTO.
	     ACCEPT PAR-SPLIT-BC AT 1955
013910		    WITH BACKGROUND-COLOR 4
			 FOREGROUND-COLOR 7 HIGHLIGHT
			 UPDATE AUTO.
	     ACCEPT PAR-HND-FEE AT 2055
013910		    WITH BACKGROUND-COLOR 4
			 FOREGROUND-COLOR 7 HIGHLIGHT
			 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING PAR-INV-CHECK
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF PAR-INV-CHECK = "Y"
	       ACCEPT S35.
	     MOVE 19		 TO SLIN.
	     PERFORM CHECK-SETUP THRU CHECK-EXIT.
013990	   IF WS-OPTION = "N"
014000         GO TO YA345.
	     CALL "CBL_TOUPPER" USING PAR-CRDX
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (PAR-CRDX = "Y")
	       MOVE "N" 	 TO PAR-CRDX.
	     CALL "CBL_TOUPPER" USING PAR-PRICED-IND
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (PAR-PRICED-IND = "Y")
	       MOVE "N" 	 TO PAR-PRICED-IND.
	     CALL "CBL_TOUPPER" USING PAR-BANK-IND
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (PAR-BANK-IND = "Y")
	       MOVE "N" 	 TO PAR-BANK-IND.
	     CALL "CBL_TOUPPER" USING PAR-PROMPT
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (PAR-PROMPT = "Y" OR "F")
	       MOVE "N" 	 TO PAR-PROMPT.
	     CALL "CBL_TOUPPER" USING PAR-SLIP-P
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (PAR-SLIP-P = "Y")
	       MOVE "N" 	 TO PAR-SLIP-P.
	     CALL "CBL_TOUPPER" USING PAR-PLU-D
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (PAR-PLU-D = "S")
	       MOVE "P" 	 TO PAR-PLU-D.
	     CALL "CBL_TOUPPER" USING PAR-BAR-CODE
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (PAR-BAR-CODE = "I")
	       MOVE "X" 	 TO PAR-BAR-CODE.
	     CALL "CBL_TOUPPER" USING PAR-OVER-LIM
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF PAR-BCSIZE < 50
	       MOVE 50		TO PAR-BCSIZE.
	     CALL "CBL_TOUPPER" USING WS-USE-GROUP
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-USE-GROUP = "Y")
	       MOVE "N" 	 TO WS-USE-GROUP.
	   IF NOT (PAR-OVER-LIM = "Y")
	       MOVE "N" 	 TO PAR-OVER-LIM.
	     CALL "CBL_TOUPPER" USING PAR-ARREARS
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (PAR-ARREARS = "Y")
	       MOVE "N" 	 TO PAR-ARREARS.
	   IF NOT (PAR-INV-CHECK = "Y")
	       MOVE "N" 	 TO PAR-INV-CHECK
	       MOVE ZERO	 TO PAR-INV-AMT
	   ELSE
	   IF PAR-INV-AMT = ZERO
	       MOVE "N" 	 TO PAR-INV-CHECK.
	     CALL "CBL_TOUPPER" USING PAR-USE-3-DEC
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (PAR-USE-3-DEC = "Y")
	       MOVE "N" 	 TO PAR-USE-3-DEC.
	     CALL "CBL_TOUPPER" USING PAR-PARCELS
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (PAR-PARCELS = "Y")
	       MOVE "N" 	 TO PAR-PARCELS.
	     CALL "CBL_TOUPPER" USING PAR-SPLIT-BC
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (PAR-SPLIT-BC = "Y")
	       MOVE "N" 	 TO PAR-SPLIT-BC.
	     CALL "CBL_TOUPPER" USING PAR-HND-FEE
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (PAR-HND-FEE = "Y")
	       MOVE "N" 	 TO PAR-HND-FEE.
	   IF (WS-OVER-LIM = "N" AND PAR-OVER-LIM = "Y") OR
	      (WS-ARREARS = "N" AND PAR-ARREARS = "Y")
	       PERFORM YA350.
	   IF PAR-HND-FEE = "Y"
	       PERFORM SAVE-SCREEN-3
	       MOVE 11		 TO SHADE-ROW
	       MOVE 25		 TO SHADE-COL
	       MOVE 33		 TO SHADE-WIDTH
	       MOVE 2		 TO SHADE-LINES
	       DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" AT 1023
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
	       DISPLAY "³ Handling Fee Percentage         ³" AT 1123
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
	       DISPLAY "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" AT 1223
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
	       PERFORM SCREEN-SHADOW
	       ACCEPT S36
	       PERFORM RESTORE-SCREEN-3.
014010	     PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
	     MOVE 5		 TO WS-PARKEY.
013680	     PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE WS-USE-GROUP	 TO PAR-USE-GROUP.
014010	     PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
	     PERFORM RESTORE-SCREEN-3.
	     GO TO YA999.

       YA350.
	     DISPLAY "Credit Limit/Arrears Password" AT 2224.
	     ACCEPT WS-PASS AT 2254
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO SECURE.
	     MOVE 6		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING WS-PASS
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-PASS = "LIMARR")
	       MOVE WS-OVER-LIM  TO PAR-OVER-LIM
	       MOVE WS-ARREARS	 TO PAR-ARREARS.

       YA999.
	     EXIT.
      /
015920 ZA000-INIT	       SECTION 8.
015930 ZA000-OPEN.
             PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
	     DISPLAY CLR-SCREEN.
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
016210	     MOVE 1		 TO WS-PARKEY.
016220       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
016230       MOVE PAR-DMY        TO W12-TODAY.
016240       MOVE PAR-YMD        TO W12-T-YMD.

       ZA28.
044780	     GO TO ZA999-EXIT.

       ZA55.
041220       MOVE 1              TO WS-S1.
021870	     MOVE SPACES	 TO WS-MID-LNE.

041240 ZA60.
041250       MOVE WS-G1          TO WS-TCHR (WS-S1) WS-BCHR (WS-S1).
	     MOVE WS-G8 	 TO WS-BCH  (WS-S1).
041260     IF WS-S1 < 80
041270         ADD 1             TO WS-S1
041280         GO TO ZA60.
	     MOVE WS-G11	 TO WS-BCH  (1).
             MOVE WS-G12         TO WS-BCH  (80).
             MOVE WS-G14         TO WS-TCHR (1)  WS-BCHR (1).
             MOVE WS-G13         TO WS-TCHR (80) WS-BCHR (80).
021930       MOVE WS-G2          TO WS-TCHR (16) WS-TCHR (41)
021940                              WS-TCHR (64) WS-TCHR (71).
021950       MOVE WS-G3          TO WS-MCHR (16) WS-MCHR (41)
021960                              WS-MCHR (64) WS-MCHR (71)
				    WS-MCHR (1)  WS-MCHR (80).
021970       MOVE WS-G4          TO WS-BCHR (16) WS-BCHR (41)
021980                              WS-BCHR (64) WS-BCHR (71).
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "SupervisorÄ"
				 TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"
				 TO WS-WRKHD
	       MOVE LS-USER	 TO WS-WRKST.

016260 ZA999-EXIT.
016270       EXIT.
      /
016280 ZB000-ERROR	     SECTION 8.
      
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
      
041560 DISPLAY-FILE-NAME.
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
045390       DISPLAY "Status " AT 1212 
                      WS-STATUS WITH FOREGROUND-COLOR 11
                     ": " WS-STAT-MESSAGE WITH FOREGROUND-COLOR 14.
045400     IF WS-STATUS NOT = "94" 
045410         DISPLAY "Key    " AT 1412 
                        WS-KEY WITH FOREGROUND-COLOR 11
045420         DISPLAY "Reverse backup or contact program Support"
                        AT 1612.
045440         DISPLAY "Please make a note of these details" AT 1812.
042170       STOP RUN.
