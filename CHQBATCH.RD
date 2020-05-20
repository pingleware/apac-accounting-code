      *
      *     ***  *   *	***  ****   ***  *   *	  ***** *** *	  *****
      *    *   * *   * *   * *	 * *   * *   *	  *	 *  *	  *
      *    *	 *   * *   * *	 * *	 *   *	  *	 *  *	  *
      *    *	 ***** *   * ****  *	 *****	  ***	 *  *	  ***
      *    *	 *   * *   * *	 * *	 *   *	  *	 *  *	  *
      *    *   * *   * *  ** *	 * *   * *   *	  *	 *  *	  *
      *     ***  *   *	**** ****   ***  *   *	  *	*** ***** *****
      *
000020 READ-CHQBATCH.
000030	     READ CHQBATCH
	       KEY IS BCH-KEY.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-CHQBATCH-EXIT.
000040     IF WS-STATUS = "23"
	       MOVE 58		 TO WS-F-ERROR
000050	       GO TO READ-CHQBATCH-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 58		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
000090     IF RECORD-LOCKED
	       MOVE W02-CHQBATCH	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000100         PERFORM LOCKED-RECORD
000110	       GO TO READ-CHQBATCH.
	     GO TO READ-CHQBATCH-EXIT.

000020 READ-CHQBATCH-NEXT.
000030	     READ CHQBATCH NEXT.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-CHQBATCH-EXIT.
000040     IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
	       MOVE 58		 TO WS-F-ERROR
000050	       GO TO READ-CHQBATCH-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 58		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
000090     IF RECORD-LOCKED
	       MOVE W02-CHQBATCH	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000100         PERFORM LOCKED-RECORD
000110	       GO TO READ-CHQBATCH-NEXT.
	     GO TO READ-CHQBATCH-EXIT.

000020 READ-CHQBATCH-PREV.
000030	     READ CHQBATCH PREVIOUS.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-CHQBATCH-EXIT.
000040     IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
	       MOVE 58		 TO WS-F-ERROR
000050	       GO TO READ-CHQBATCH-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 58		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
000090     IF RECORD-LOCKED
	       MOVE W02-CHQBATCH	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000100         PERFORM LOCKED-RECORD
000110	       GO TO READ-CHQBATCH-PREV.
	     GO TO READ-CHQBATCH-EXIT.

       START-AT-CHQBATCH-KEY.
000030	     START CHQBATCH
	       KEY >= BCH-KEY.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-CHQBATCH-EXIT.
000040     IF WS-STATUS = "23"
	       MOVE 58		 TO WS-F-ERROR
000050	       GO TO READ-CHQBATCH-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 58		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
	     GO TO READ-CHQBATCH-EXIT.

000150 READ-CHQBATCH-EXIT.
000160       EXIT.
