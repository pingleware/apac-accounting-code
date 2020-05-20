      *
      *    ****  ****  ****  *****  ***  *   *	  ***** *** *	  *****
      *    *   * *   * *   *   *   *   * *   *	  *	 *  *	  *
      *    *   * *   * *   *   *   *	 *   *	  *	 *  *	  *
      *    *   * ****  ****    *   *	 *****	  ***	 *  *	  ***
      *    *   * *   * *   *   *   *	 *   *	  *	 *  *	  *
      *    *   * *   * *   *   *   *   * *   *	  *	 *  *	  *
      *    ****  *   * ****    *    ***  *   *	  *	*** ***** *****
      *
000020 READ-DRBTCH.
000030	     READ DRBTCH
	       KEY IS DRB-KEY.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-DRBTCH-EXIT.
000040     IF WS-STATUS = "23"
	       MOVE 42		 TO WS-F-ERROR
000050	       GO TO READ-DRBTCH-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 42		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
000090     IF RECORD-LOCKED
	       MOVE W02-DRBTCH	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000100         PERFORM LOCKED-RECORD
000110	       GO TO READ-DRBTCH.
	     GO TO READ-DRBTCH-EXIT.

000020 READ-DRBTCH-NEXT.
000030	     READ DRBTCH NEXT.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-DRBTCH-EXIT.
000040     IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
	       MOVE 42		 TO WS-F-ERROR
000050	       GO TO READ-DRBTCH-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 42		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
000090     IF RECORD-LOCKED
	       MOVE W02-DRBTCH	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000100         PERFORM LOCKED-RECORD
000110	       GO TO READ-DRBTCH-NEXT.
	     GO TO READ-DRBTCH-EXIT.

000020 READ-DRBTCH-PREV.
000030	     READ DRBTCH PREVIOUS.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-DRBTCH-EXIT.
000040     IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
	       MOVE 42		 TO WS-F-ERROR
000050	       GO TO READ-DRBTCH-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 42		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
000090     IF RECORD-LOCKED
	       MOVE W02-DRBTCH	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000100         PERFORM LOCKED-RECORD
000110	       GO TO READ-DRBTCH-PREV.
	     GO TO READ-DRBTCH-EXIT.

       START-AT-DRBTCH-KEY.
000030	     START DRBTCH
	       KEY >= DRB-KEY.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-DRBTCH-EXIT.
000040     IF WS-STATUS = "23"
	       MOVE 42		 TO WS-F-ERROR
000050	       GO TO READ-DRBTCH-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 42		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
	     GO TO READ-DRBTCH-EXIT.

000150 READ-DRBTCH-EXIT.
000160       EXIT.
