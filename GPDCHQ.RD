      *
      *    ****  ****	***  *	 * *****  ***	  ***** *** *	  *****
      *    *   * *   * *   * *	 * *	 *   *	  *	 *  *	  *
      *    *   * *   * *     *	 * *	 *   *	  *	 *  *	  *
      *    ****  *   * *     ***** ***	 *   *	  ***	 *  *	  ***
      *    *	 *   * *     *	 * *	 * * *	  *	 *  *	  *
      *    *	 *   * *   * *	 * *	  ***	  *	 *  *	  *
      *    *	 ****	***  *	 * *****     *	  *	*** ***** *****
      *
008370 READ-GPDCHQ.
008380	     READ GPDCHQ
	       KEY IS GPD-KEY.
008390     IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
008400	       GO TO READ-GPDCHQ-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
008420	       MOVE 45		 TO WS-F-ERROR
008400	       GO TO READ-GPDCHQ-EXIT.
008410     IF WS-STAT1 = "2" OR "3" OR "4"
008420	       MOVE 45		 TO WS-F-ERROR
008430         PERFORM READ-ERROR.
008440     IF RECORD-LOCKED
	       MOVE W02-GPDCHQ	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
008450         PERFORM LOCKED-RECORD
008460	       GO TO READ-GPDCHQ.
008470	     MOVE 45		 TO WS-F-ERROR.
008480       PERFORM READ-ERROR.
	     GO TO READ-GPDCHQ-EXIT.

008370 READ-GPDCHQ-NEXT.
008380	     READ GPDCHQ NEXT.
008390     IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
008400	       GO TO READ-GPDCHQ-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
008420	       MOVE 45		 TO WS-F-ERROR
008400	       GO TO READ-GPDCHQ-EXIT.
008410     IF WS-STAT1 = "2" OR "3" OR "4"
008420	       MOVE 45		 TO WS-F-ERROR
008430         PERFORM READ-ERROR.
008440     IF RECORD-LOCKED
	       MOVE W02-GPDCHQ	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
008450         PERFORM LOCKED-RECORD
008460	       GO TO READ-GPDCHQ-NEXT.
008470	     MOVE 45		 TO WS-F-ERROR.
008480       PERFORM READ-ERROR.
	     GO TO READ-GPDCHQ-EXIT.

008370 READ-GPDCHQ-PREV.
008380	     READ GPDCHQ PREVIOUS.
008390     IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
008400	       GO TO READ-GPDCHQ-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
008420	       MOVE 45		 TO WS-F-ERROR
008400	       GO TO READ-GPDCHQ-EXIT.
008410     IF WS-STAT1 = "2" OR "3" OR "4"
008420	       MOVE 45		 TO WS-F-ERROR
008430         PERFORM READ-ERROR.
008440     IF RECORD-LOCKED
	       MOVE W02-GPDCHQ	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
008450         PERFORM LOCKED-RECORD
008460	       GO TO READ-GPDCHQ-PREV.
008470	     MOVE 45		 TO WS-F-ERROR.
008480       PERFORM READ-ERROR.
	     GO TO READ-GPDCHQ-EXIT.

008370 START-AT-GPDCHQ-KEY.
008380	     START GPDCHQ
	       KEY >= GPD-KEY.
008390     IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
008400	       GO TO READ-GPDCHQ-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
008420	       MOVE 45		 TO WS-F-ERROR
008400	       GO TO READ-GPDCHQ-EXIT.
008410     IF WS-STAT1 = "2" OR "3" OR "4"
008420	       MOVE 45		 TO WS-F-ERROR
008430         PERFORM READ-ERROR.
	     GO TO READ-GPDCHQ-EXIT.

008370* START-AT-GPDCHQ-GL.
008380*	     START GPDCHQ
      *	       KEY >= GPD-GL.
008390*     IF WS-STAT1 = "0"
      * 	MOVE ZERO	  TO WS-F-ERROR
008400*	       GO TO READ-GPDCHQ-EXIT.
      *     IF (WS-STATUS = "23") OR
      *        (WS-STAT1 = "1")
008420*	       MOVE 45		 TO WS-F-ERROR
008400*	       GO TO READ-GPDCHQ-EXIT.
008410*     IF WS-STAT1 = "2" OR "3" OR "4"
008420*	       MOVE 45		 TO WS-F-ERROR
008430* 	PERFORM READ-ERROR.
      *
008370 START-AT-GPDCHQ-BANK.
008380	     START GPDCHQ
	       KEY >= GPD-BNK.
008390     IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
008400	       GO TO READ-GPDCHQ-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
008420	       MOVE 45		 TO WS-F-ERROR
008400	       GO TO READ-GPDCHQ-EXIT.
008410     IF WS-STAT1 = "2" OR "3" OR "4"
008420	       MOVE 45		 TO WS-F-ERROR
008430         PERFORM READ-ERROR.

008490 READ-GPDCHQ-EXIT.
008500       EXIT.
