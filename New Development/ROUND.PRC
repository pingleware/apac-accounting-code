      /
      *    R O U N D I N G   O F   S E L L I N G   P R I C E S
      *
      *    This routine will round the selling price as follows:
      *
      *    If the rounding factor is less than 50 cents or greater
      *    than R1.00 but less than five rand, then the selling price
      *    (.CC or R.CC portion) will be rounded to a multiple of the
      *    rounding factor.
      *
      *    If the rounding factor is R1.00, then if the selling price
      *    is greater than 79 cents it will be rounded to the next rand.
      *
      *    If the rounding factor is greater than 49 cents and less than
      *    R1.00 or greater than R4.99 then the selling price (.CC or
      *    R.CC portion) will be changed to the rounding value. If the
      *    rounding portion of the selling price is greater than the
      *    factor then the system will amend these figures and change
      *    the adjoining value.
      *
       CB100         SECTION.
       CB101.
      *
      *    ****    I F	 T H E   V A L U E   I S   Z E R O  -  E X I T
      *
           IF W10-RSELL = ZERO
               GO TO CB199.
      *
      *    ROUND TO THE NEXT RAND
      *
           IF W05-ROUND-VAL = 1.00
               IF W10-RSC = ZERO
                   GO TO CB199
               ELSE
      *
      *    SELLING PRICE LESS THAN ONE RAND
      *
               IF W10-RSELL < 1.00
      *
      *    SELLING PRICE IS MORE THAN 79 CENTS - CHANGE TO R1.00
      *
                   IF W10-RSC > 79
                       MOVE 1.00     TO W10-RSELL
                       GO TO CB199
                   ELSE
      *
      *    IF THE SELLING PRICE IS LESS THAN 80 CENTS DO NOT ROUND UP
      *
                       GO TO CB199
      *
      *    SELLING PRICE MORE THAN ONE RAND
      *
               ELSE
      *
      *    CENTS PORTION OF PRICE IS LESS THAN TEN CENTS - DROP CENTS
      *
                   IF W10-RSC < 10
                       MOVE ZERO     TO W10-RSC
                       GO TO CB199
                   ELSE
      *
      *    ROUND UP TO NEXT R1.00
      *
                       MOVE ZERO     TO W10-RSC
                       ADD 1         TO W10-RSR
                       GO TO CB199.
      *
      *    ROUNDING CENTS ONLY
      *
           IF W05-ROUND-VAL < 1.00
      *
      *    ROUNDING VALUE AND CENTS PORTION ARE EQUAL - EXIT
      *
               IF W10-RSC = W05-CENTS
                   GO TO CB199
               ELSE
      *
      *    ROUNDING VALUE IS 50 CENTS OR MORE
      *
               IF W05-ROUND-VAL > .50
      *
      *    IF CENTS PORTION TO BE ROUNDED IS LESS THAN ROUNDING VALUE
      *
                   IF W10-RSC < W05-CENTS
                       MOVE W05-CENTS  TO W10-RSC
                       GO TO CB199
      *
      *    IF CENTS PORTION TO BE ROUNDED IS MORE THAN ROUNDING VALUE
      *
                   ELSE
                       MOVE W05-CENTS  TO W10-RSC
                       ADD 1           TO W10-RSR
                       GO TO CB199
      *
      *    ROUNDING VALUE IS LESS THAN 50 CENTS
      *
               ELSE
                   GO TO CB120.
      *
      *    IF ROUNDING VALUE EQUALS THE R.CC OF VALUE TO BE ROUNDED - EXIT
      *
           IF W10-RSRCC = W05-ROUND-VAL
               GO TO CB199.
      *
      *    SELLING PRICE IS NOT MORE THAN HALF THE ROUNDING FACTOR - EXIT
      *
           IF W10-RSELL NOT > W05-HLF-VAL
               GO TO CB199.
           IF W05-ROUND-VAL > 5.00
               GO TO CB110.
      *
      *    ROUNDING FACTOR IS LESS THAN FIVE RAND
      *
      *    R.CC OF VALUE TO BE ROUNDED IS LESS THAN ROUNDING FACTOR
      *
           IF W10-RSRCC < W05-ROUND-VAL
               MOVE W05-ROUND-VAL    TO W10-RSRCC
               GO TO CB199.
             MOVE W05-ROUND-VAL      TO W05-CALC-VAL.
       CB105.
           IF W10-RSRCC = W05-CALC-VAL
               GO TO CB199.
           IF W10-RSRCC < W05-CALC-VAL
               MOVE W05-CALC-VAL     TO W10-RSRCC
               GO TO CB199.
             ADD W05-ROUND-VAL       TO W05-CALC-VAL.
           IF W05-CALC-VAL < 10.00
               GO TO CB105.
             ADD 1                   TO W10-RSR8.
             MOVE W05-RVRCC          TO W10-RSRCC.
             GO TO CB199.
      *
      *    ROUNDING FACTOR GREATER THAN R5.00
      *
      *    SELLING PRICE IS NOT MORE THAN 75% OF ROUNDING FACTOR - EXIT
      *
       CB110.
           IF W10-RSELL NOT > W05-TQTR-VAL
               GO TO CB199.
       CB115.
           IF W10-RSRCC < W05-ROUND-VAL
               MOVE W05-ROUND-VAL    TO W10-RSRCC
               GO TO CB199.
             ADD 1                   TO W10-RSR8.
             MOVE W05-ROUND-VAL      TO W10-RSRCC.
             GO TO CB199.
      *
      *    ROUND UP VALUES WHERE ROUNDING FACTOR IS LESS THAN 50 CENTS
      *
       CB120.
	   IF W05-CENTS = 2 OR 4 OR 5 OR 10 OR 20 OR 25
             MOVE ZERO               TO WS-SUB
	   ELSE
             MOVE W05-CENTS          TO WS-SUB.
       CB125.
           IF W10-RSC = WS-SUB
               GO TO CB199.
           IF W10-RSC < WS-SUB
               MOVE WS-SUB           TO W10-RSC
               GO TO CB199.
             ADD W05-CENTS           TO WS-SUB.
           IF WS-SUB < 100
               GO TO CB125.
             ADD 1                   TO W10-RSR.
             SUBTRACT 100 FROM WS-SUB  GIVING W10-RSC.
       CB199.
             EXIT.
      *
      *    ****    C A L C U L A T E   E X C L U S I V E   P R I C E
      *              F R O M   T H E   I N C L U S I V E   P R I C E
      *
       CB200         SECTION.
       CB201.
           IF STK-TAX NOT = ZERO
               COMPUTE W10-SELL ROUNDED = (W10-VSELL / (100.00000 + W05-RATE)) * 100.00
               COMPUTE W10-WSALE ROUNDED = (W10-VWSALE / (100.00000 + W05-RATE)) * 100.00
               COMPUTE W10-CASH ROUNDED = (W10-VCASH / (100.00000 + W05-RATE)) * 100.00
           ELSE
               MOVE W10-VWSALE       TO W10-WSALE
               MOVE W10-VCASH        TO W10-CASH
               MOVE W10-VSELL        TO W10-SELL.
           IF STK-TAX NOT = ZERO
               COMPUTE W10-CHECK ROUNDED = W10-SELL + (W10-SELL * W05-RTE)
               IF W10-CHECK > W10-VSELL
                   SUBTRACT 0.01     FROM W10-SELL
               ELSE
               IF W10-CHECK < W10-VSELL
                   ADD 0.01	       TO W10-SELL
	         END-IF
               COMPUTE W10-CHECK ROUNDED = W10-CASH + (W10-CASH * W05-RTE)
	         IF W10-CHECK > W10-VCASH
                   SUBTRACT 0.01     FROM W10-CASH
               ELSE
               IF W10-CHECK < W10-VCASH
                   ADD 0.01          TO W10-CASH
	       END-IF
	       COMPUTE W10-CHECK ROUNDED = W10-WSALE + (W10-WSALE * W05-RTE)
             IF W10-CHECK > W10-VWSALE
                 SUBTRACT 0.01       FROM W10-WSALE
             ELSE
             IF W10-CHECK < W10-VWSALE
                 ADD 0.01            TO W10-WSALE.
       CB299.
             EXIT.
