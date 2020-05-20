      *
      *    ****  ****	***  *	 * *****  ***	  ***** *** *	  *****
      *    *   * *   * *   * *	 * *	 *   *	  *	 *  *	  *
      *    *   * *   * *     *	 * *	 *   *	  *	 *  *	  *
      *    ****  *   * *     ***** ***	 *   *	  ***	 *  *	  ***
      *    *	 *   * *     *	 * *	 * * *	  *	 *  *	  *
      *    *	 *   * *   * *	 * *	  ***	  *	 *  *	  *
      *    *	 ****	***  *	 * *****     *	  *	*** ***** *****
      *
      *==================================================================
      *
      *    The routine READ-ERROR will be performed if a status other than
      *    zero is returned after any READ or START request and requires
      *    that the file number to be moved to WS-F-ERROR before being
      *    performed.
      *
      *    On returning from READ-ERROR, WS-ERROR will contain a value as
      *    follows:
      * 	    0 - No error or status to report
      * 	    1 - Record locked
      * 	    2 - Duplicate key
      * 	    3 - No Record / End of file / Invalid key
      * 	    4 - File already open
      * 	    5 - File already closed
      * 	    6 - Index corrupted
      * 	    7 - Network / Communication error
      * 	    8 - Limit exceeded - Record locks
      * 	    9 - File limit exceeded (files open)
      * 	   10 - Open for output
      * 	   11 - Open for input
      * 	   12 - File locked
      * 	   13 - File not open
      * 	   14 - File not open for I/O
      * 	   99 - All other errors
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
