      *
      *    ****  ****  ***  ***  *****	***	 ***** *** *	 *****
      *    *   * *   *	*  *   * *     *   *	 *	*  *	 *
      *    *   * *   *	*  *	 *     *	 *	*  *	 *
      *    ****  ****	*  *	 ***	***	 ***	*  *	 ***
      *    *	 *   *	*  *	 *	   *	 *	*  *	 *
      *    *	 *   *	*  *   * *     *   *	 *	*  *	 *
      *    *	 *   * ***  ***  *****	***	 *     *** ***** *****
      *
006080 READ-PRICES.
006090	     READ PRICES	 WITH IGNORE LOCK
	       KEY IS PRC-CODE.
006100     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110	       GO TO READ-PRICES-EXIT.
           IF WS-STATUS = "23"
	       MOVE 1		 TO WS-F-ERROR
	       GO TO READ-PRICES-EXIT.
006120     IF WS-STAT1 = "2" OR "3" OR "4"
006130	       MOVE 1		 TO WS-F-ERROR
006140         PERFORM READ-ERROR.
006200	     GO TO READ-PRICES-EXIT.

006080 READ-PRICES-NEXT.
006090	     READ PRICES NEXT.
006100     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110	       GO TO READ-PRICES-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
	       MOVE 1		 TO WS-F-ERROR
	       GO TO READ-PRICES-EXIT.
006120     IF WS-STAT1 = "2" OR "3" OR "4"
006130	       MOVE 1		 TO WS-F-ERROR
006140         PERFORM READ-ERROR.
006200	     GO TO READ-PRICES-EXIT.

006080 READ-PRICES-PREV.
006090	     READ PRICES PREVIOUS.
006100     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110	       GO TO READ-PRICES-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
	       MOVE 1		 TO WS-F-ERROR
	       GO TO READ-PRICES-EXIT.
006120     IF WS-STAT1 = "2" OR "3" OR "4"
006130	       MOVE 1		 TO WS-F-ERROR
006140         PERFORM READ-ERROR.
006200	     GO TO READ-PRICES-EXIT.

006080 START-AT-PRC-CODE.
006090	     START PRICES
	       KEY >= PRC-CODE.
006100     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110	       GO TO READ-PRICES-EXIT.
           IF WS-STATUS = "23"
	       MOVE 1		 TO WS-F-ERROR
	       GO TO READ-PRICES-EXIT.
006120     IF WS-STAT1 = "2" OR "3" OR "4"
006130	       MOVE 1		 TO WS-F-ERROR
006140         PERFORM READ-ERROR.
006200	     GO TO READ-PRICES-EXIT.

006080 START-AT-PRC-DESC1.
006090	     START PRICES
	       KEY >= PRC-DKEY.
006100     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110	       GO TO READ-PRICES-EXIT.
           IF WS-STATUS = "23"
	       MOVE 1		 TO WS-F-ERROR
	       GO TO READ-PRICES-EXIT.
006120     IF WS-STAT1 = "2" OR "3" OR "4"
006130	       MOVE 1		 TO WS-F-ERROR
006140         PERFORM READ-ERROR.
006200	     GO TO READ-PRICES-EXIT.

006080 START-AT-PRC-DESC2.
006090	     START PRICES
	       KEY >= PRC-DKEY2.
006100     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110	       GO TO READ-PRICES-EXIT.
           IF WS-STATUS = "23"
	       MOVE 1		 TO WS-F-ERROR
	       GO TO READ-PRICES-EXIT.
006120     IF WS-STAT1 = "2" OR "3" OR "4"
006130	       MOVE 1		 TO WS-F-ERROR
006140         PERFORM READ-ERROR.
006200	     GO TO READ-PRICES-EXIT.

006080 START-AT-PRC-XREF.
006090	     START PRICES
	       KEY >= PRC-XREF.
006100     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110	       GO TO READ-PRICES-EXIT.
           IF WS-STATUS = "23"
	       MOVE 1		 TO WS-F-ERROR
	       GO TO READ-PRICES-EXIT.
006120     IF WS-STAT1 = "2" OR "3" OR "4"
006130	       MOVE 1		 TO WS-F-ERROR
006140         PERFORM READ-ERROR.

006520 READ-PRICES-EXIT.
006530       EXIT.
