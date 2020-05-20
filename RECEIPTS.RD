      *
      *    ****  *****	***  ***** *** ****  *****  ***
      *    *   * *     *   * *	    *  *   *   *   *   *
      *    *   * *     *     *	    *  *   *   *   *
      *    ****  ****  *     ****   *  ****    *    ***
      *    *   * *     *     *	    *  *       *       *
      *    *   * *     *   * *	    *  *       *   *   *
      *    *   * *****	***  ***** *** *       *    ***
      *
000020 READ-RECEIPTS.
000030	     READ RECEIPTS
	       KEY IS RCT-REF.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-RECEIPTS-EXIT.
           IF WS-STATUS = "23"
	       MOVE 90		 TO WS-F-ERROR
000050	       GO TO READ-RECEIPTS-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 90		  TO WS-F-ERROR
000080         PERFORM READ-ERROR.
000090     IF RECORD-LOCKED
	       MOVE W02-RECEIPTS	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000100         PERFORM LOCKED-RECORD
000110	       GO TO READ-RECEIPTS.
000050	     GO TO READ-RECEIPTS-EXIT.

000020 READ-RECEIPTS-NEXT.
000030	     READ RECEIPTS NEXT.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-RECEIPTS-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
	       MOVE 90		 TO WS-F-ERROR
000050	       GO TO READ-RECEIPTS-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 90		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
000090     IF RECORD-LOCKED
	       MOVE W02-RECEIPTS TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000100         PERFORM LOCKED-RECORD
000110	       GO TO READ-RECEIPTS-NEXT.
000050	     GO TO READ-RECEIPTS-EXIT.

000020 READ-RECEIPTS-PREV.
000030	     READ RECEIPTS PREVIOUS.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-RECEIPTS-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
	       MOVE 90		 TO WS-F-ERROR
000050	       GO TO READ-RECEIPTS-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 90		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
000090     IF RECORD-LOCKED
	       MOVE W02-RECEIPTS TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000100         PERFORM LOCKED-RECORD
000110	       GO TO READ-RECEIPTS-PREV.
000050	     GO TO READ-RECEIPTS-EXIT.

000020 START-AT-RECT-REF.
000030	     START RECEIPTS
	       KEY >= RCT-REF.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-RECEIPTS-EXIT.
           IF WS-STATUS = "23"
	       MOVE 90		 TO WS-F-ERROR
000050	       GO TO READ-RECEIPTS-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 90		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
	     GO TO READ-RECEIPTS-EXIT.

000020 START-AT-RECT-AC.
000030	     START RECEIPTS
	       KEY >= RCT-AKEY.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-RECEIPTS-EXIT.
           IF WS-STATUS = "23"
	       MOVE 90		 TO WS-F-ERROR
000050	       GO TO READ-RECEIPTS-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 90		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
	     GO TO READ-RECEIPTS-EXIT.

000020 START-AT-RECT-TYPE.
000030	     START RECEIPTS
	       KEY >= RCT-TKEY.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-RECEIPTS-EXIT.
           IF WS-STATUS = "23"
	       MOVE 90		 TO WS-F-ERROR
000050	       GO TO READ-RECEIPTS-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 90		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.

000150 READ-RECEIPTS-EXIT.
000160       EXIT.
