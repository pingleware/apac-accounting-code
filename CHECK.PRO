      *
      *    ****   This routine calculates the check-digit for the
      * 	  price portion of the Bar-code and then the overall
      * 	  check-digit of the Bar-code.
      *
      * 	  The routine uses three tables that contain the
      * 	  2-(W80-2N), 5-(W80-5N) and 5+(W80-5P) weighted
      * 	  products for the digits 0 through 9.
      *
      * 	  As digit 0 always produces a weighted product of
      * 	  zero for all the tables, only digits 1 to 9 are
      * 	  included in the tables.
      *
      * 	  The product is obtained by using the digit in the
      * 	  price as a subscript in the table that applies to
      * 	  the digit position.
      *
       YZ000-PRICE-CHECK-DIGIT	SECTION 79.
       YZ00-CALC-PRC-CD.
	     MOVE STK-CASH	 TO W80-PRICE.
	     MOVE ZERO		 TO W80-WGHTS.
	   IF W80-PRC-DIG(1) > ZERO
	       MOVE W80-PRC-DIG(1)
				 TO W80-SUB
	       MOVE W80-5P(W80-SUB)
				 TO W80-WEIGHT(1).
	   IF W80-PRC-DIG(2) > ZERO
	       MOVE W80-PRC-DIG(2)
				 TO W80-SUB
	       MOVE W80-2N(W80-SUB)
				 TO W80-WEIGHT(2).
	   IF W80-PRC-DIG(3) > ZERO
	       MOVE W80-PRC-DIG(3)
				 TO W80-SUB
	       MOVE W80-5N(W80-SUB)
				 TO W80-WEIGHT(3).
	   IF W80-PRC-DIG(4) > ZERO
	       MOVE W80-PRC-DIG(4)
				 TO W80-SUB
	       MOVE W80-5P(W80-SUB)
				 TO W80-WEIGHT(4).
	   IF W80-PRC-DIG(5) > ZERO
	       MOVE W80-PRC-DIG(5)
				 TO W80-SUB
	       MOVE W80-2N(W80-SUB)
				 TO W80-WEIGHT(5).
	     ADD W80-WEIGTHS(1) W80-WEIGHTS(2) W80-WEIGHTS(3)
		 W80-WEIGHTS(4) W80-WEIGHTS(5) GIVING W80-RESULT.
	   IF W80-UNIT > ZERO
	       SUBTRACT W80-UNIT FROM 10
				 GIVING W80-SUB
	   ELSE
	       MOVE ZERO	 TO W80-SUB.
	   IF W80-SUB = ZERO
	       MOVE ZERO	 TO W80-CD
	       GO TO YZ10-PRC-CD-FOUND.
	     MOVE 1		 TO W80-CD.

       YZ05-FIND-PRC-CD.
	   IF W80-SUB = W80-5N(W80-CD)
	       GO TO YZ10-PRC-CD-FOUND.
	     ADD 1		 TO W80-CD.
	   IF W80-CD < 9
	       GO TO YZ05-FIND-PRC-CD.

       YZ10-PRC-CD-FOUND.
	     MOVE W80-CD	 TO W80-PRC-CD.
      *
      *    ****    C A L C U L A T E   T H E   B A R - C O D E
      * 	   C H E C K   D I G I T
      *
       YZ15-CALC-CHECK-DIGIT.
	     ADD W80-BAR-CODE-CH (1),
		 W80-BAR-CODE-CH (3),
		 W80-BAR-CODE-CH (5),
		 W80-BAR-CODE-CH (7),
		 W80-BAR-CODE-CH (9),
		 W80-BAR-CODE-CH (11),
				 GIVING W80-BAR-CODE-RESULT-1.
	     ADD W80-BAR-CODE-CH (2),
		 W80-BAR-CODE-CH (4),
		 W80-BAR-CODE-CH (6),
		 W80-BAR-CODE-CH (8),
		 W80-BAR-CODE-CH (10),
		 W80-BAR-CODE-CH (12)
				 GIVING W80-BAR-CODE-RESULT-2.
	     MULTIPLY 3 	 BY W80-BAR-CODE-RESULT-2.
	     ADD W80-BAR-CODE-RESULT-1
		 W80-BAR-CODE-RESULT-2
				 GIVING W80-BAR-CODE-RESULT-3.
	   IF W80-BAR-CODE-UNIT > ZERO
	       SUBTRACT W80-BAR-CODE-UNIT FROM 10
				 GIVING W80-BAR-CODE-CHK-DIG
	   ELSE
	       MOVE ZERO	 TO W80-BAR-CODE-CHK-DIG.
