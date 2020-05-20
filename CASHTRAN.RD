      *
      *     ***   ***  ***** ****    *	 *   *	  ***** *** *	  *****
      *    *   * *   *	 *   *	 *  * *  *   *	  *	 *  *	  *
      *    *	 *	 *   *	 * *   * **  *	  *	 *  *	  *
      *    *	  ***	 *   ****  ***** * * *	  ***	 *  *	  ***
      *    *	     *	 *   *	 * *   * *  **	  *	 *  *	  *
      *    *   * *   *	 *   *	 * *   * *   *	  *	 *  *	  *
      *     ***   ***	 *   *	 * *   * *   *	  *	*** ***** *****
      *
008370 READ-CASHTR.
008380	     READ CASHTRAN
	       KEY IS CTR-KEY.
008390     IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
008400	       GO TO READ-CASHTR-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
008420	       MOVE 30		 TO WS-F-ERROR
008400	       GO TO READ-CASHTR-EXIT.
008410     IF WS-STAT1 = "2" OR "3" OR "4"
008420	       MOVE 30		 TO WS-F-ERROR
008430         PERFORM READ-ERROR.
008440     IF RECORD-LOCKED
	       MOVE W02-CASHTRAN TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
008450         PERFORM LOCKED-RECORD
008460	       GO TO READ-CASHTR.
008470	     MOVE 30		 TO WS-F-ERROR.
008480       PERFORM READ-ERROR.
	     GO TO READ-CASHTR-EXIT.

008370 READ-CASHTR-NEXT.
008380	     READ CASHTRAN NEXT.
008390     IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
008400	       GO TO READ-CASHTR-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
008420	       MOVE 30		 TO WS-F-ERROR
008400	       GO TO READ-CASHTR-EXIT.
008410     IF WS-STAT1 = "2" OR "3" OR "4"
008420	       MOVE 30		 TO WS-F-ERROR
008430         PERFORM READ-ERROR.
008440     IF RECORD-LOCKED
	       MOVE W02-CASHTRAN TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
008450         PERFORM LOCKED-RECORD
008460	       GO TO READ-CASHTR-NEXT.
008470	     MOVE 30		 TO WS-F-ERROR.
008480       PERFORM READ-ERROR.
	     GO TO READ-CASHTR-EXIT.

008370 READ-CASHTR-PREV.
008380	     READ CASHTRAN PREVIOUS.
008390     IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
008400	       GO TO READ-CASHTR-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
008420	       MOVE 30		 TO WS-F-ERROR
008400	       GO TO READ-CASHTR-EXIT.
008410     IF WS-STAT1 = "2" OR "3" OR "4"
008420	       MOVE 30		 TO WS-F-ERROR
008430         PERFORM READ-ERROR.
008440     IF RECORD-LOCKED
	       MOVE W02-CASHTRAN TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
008450         PERFORM LOCKED-RECORD
008460	       GO TO READ-CASHTR-PREV.
008470	     MOVE 30		 TO WS-F-ERROR.
008480       PERFORM READ-ERROR.
	     GO TO READ-CASHTR-EXIT.

008370 START-AT-CASHTR-KEY.
008380	     START CASHTRAN
	       KEY >= CTR-KEY.
008390     IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
008400	       GO TO READ-CASHTR-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
008420	       MOVE 30		 TO WS-F-ERROR
008400	       GO TO READ-CASHTR-EXIT.
008410     IF WS-STAT1 = "2" OR "3" OR "4"
008420	       MOVE 30		 TO WS-F-ERROR
008430         PERFORM READ-ERROR.
	     GO TO READ-CASHTR-EXIT.

008370 START-AT-CASHTR-AKEY.
008380	     START CASHTRAN
	       KEY >= CTR-AKEY.
008390     IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
008400	       GO TO READ-CASHTR-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
008420	       MOVE 30		 TO WS-F-ERROR
008400	       GO TO READ-CASHTR-EXIT.
008410     IF WS-STAT1 = "2" OR "3" OR "4"
008420	       MOVE 30		 TO WS-F-ERROR
008430         PERFORM READ-ERROR.
	     GO TO READ-CASHTR-EXIT.

008370 START-AT-CASHTR-VKEY.
008380	     START CASHTRAN
	       KEY >= CTR-VKEY.
008390     IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
008400	       GO TO READ-CASHTR-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
008420	       MOVE 30		 TO WS-F-ERROR
008400	       GO TO READ-CASHTR-EXIT.
008410     IF WS-STAT1 = "2" OR "3" OR "4"
008420	       MOVE 30		 TO WS-F-ERROR
008430         PERFORM READ-ERROR.

008490 READ-CASHTR-EXIT.
008500       EXIT.
