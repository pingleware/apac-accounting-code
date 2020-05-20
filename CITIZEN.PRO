     *
     *	   C I T I Z E N   C P L   B A R   C O D E   P R I N T I N G
     *
     *	   STX X
     *	   ³   À Command (see below)
     *	   ÀÄÄÄÄ [02] All commands used in this routine will start with
     *		      an STX (Start of text) character.
     *
     *	   COMMANDS: E9999 (number of prints for edited mode)
     *		     G	   Print edited format
     *		     L	   Set label format
     *		     M9999 Maximum label length
     *		     m	   Change units to metric
     *		     n	   Change units to inches
     *
     *	   9 X 9 9 999 9999 9999 XXXXXXXXXXXXXX
     *	   ³ ³ ³ ³  ³	³    ³	 À Print characters (bar code)
     *	   ³ ³ ³ ³  ³	³    ÀÄÄÄÄ Column address (distance right)
     *	   ³ ³ ³ ³  ³	ÀÄÄÄÄÄÄÄÄÄ Row address (distance upward)
     *	   ³ ³ ³ ³  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄ Bar code height
     *	   ³ ³ ³ ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ Narrow bar width
     *	   ³ ³ ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ Wide bar width
     *	   ³ ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ Bar code selection (see below)
     *	   ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ Printing angle (1=0,2=90,3=180,4=270)
     *
     *	   Bar codes: A - 3 OF 9	   M - UPC 2 DIG ADD
     *		      B - UPC-A 	   N - UPC 5 DIG ADD
     *		      C - UPC-E 	   O - CODE 93
     *		      D - 12 OF 5	   p - ZIP
     *		      E - CODE 128 (B)	   Q - UCC/EAN 128
     *		      F - EAN-13	   R - UCC/EAN 128 (KMART)
     *		      G - EAN-8 	   S - UCC/EAN 128 (RANDOM)
     *		      H - HIBC		   T - TELEPEN
     *		      I - CODEBAR	   u - UPS MAXICODE
     *		      J - 12 OF 5 W/BARS   v - FIM
     *		      K - PLESSEY	   z - PDF417
     *		      L - CASECODE
     *
       BA200.
	     MOVE SPACES	 TO REP-L1.
	   IF NOT (W15-ITEM = W15-CODE)
	       MOVE W15-ITEM	 TO W30-BAR-CDE
	       IF W15-PRICE > 99.99
		   MOVE W15-PRICE
				 TO W30-VALUE
	       ELSE
		   MOVE W15-PRICE
				 TO W30-VALUE2
	       END-IF
	       WRITE REP-L1 BEFORE 1 LINES.
	     MOVE W15-DESC	 TO W30-DESC.
	     MOVE W30-SETUP	 TO REP-CIT-CONT.

	   IF WS-BC-STYLE = "1"
	       MOVE "B" 	 TO W30-BC-TYPE
	   ELSE
	   IF WS-BC-STYLE = "2"
	       MOVE "C"		 TO W30-BC-TYPE
	   ELSE
	   IF WS-BC-STYLE = "3"
	       MOVE "F" 	 TO W30-BC-TYPE
	   ELSE
	   IF WS-BC-STYLE = "4"
	       MOVE "G" 	 TO W30-BC-TYPE
	   ELSE
	   IF WS-BC-STYLE = "5"
	       MOVE "A" 	 TO W30-BC-TYPE
	   ELSE
	   IF WS-BC-STYLE = "6"
	       MOVE "D" 	 TO W30-BC-TYPE
	   ELSE
	       MOVE "I" 	 TO W30-BC-TYPE.
	     MOVE W30-BAR-CODE	 TO REP-CIT-BCDE.
	     WRITE REP-L1 BEFORE 1 LINES.
	     MOVE W30-DESCRIPTION
				 TO REP-L4.
	     WRITE REP-L1 BEFORE 1 LINES.
	   IF W15-PRICE > 99.99
	       MOVE W30-PRICE	 TO REP-L4
	   ELSE
	       MOVE W30-PRICE2	 TO REP-L4.
	     WRITE REP-L1 BEFORE 1 LINES.
	     MOVE "X"		 TO REP-L4.
	     WRITE REP-L1 BEFORE 1 LINES.
	     MOVE W15-QNT	 TO W30-NUMBER.
	     WRITE REP-L1 BEFORE 1 LINES.

       BA299.
	     EXIT.
