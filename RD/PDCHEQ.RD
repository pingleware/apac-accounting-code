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
008370 READ-PDCHEQ.
008380	     READ PDCHEQ
	       KEY IS PDT-KEY.
008390     IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
008400	       GO TO READ-PDCHEQ-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
008420	       MOVE 44		 TO WS-F-ERROR
008400	       GO TO READ-PDCHEQ-EXIT.
008410     IF WS-STAT1 = "2" OR "3" OR "4"
008420	       MOVE 44		 TO WS-F-ERROR
008430         PERFORM READ-ERROR.
008440     IF RECORD-LOCKED
	       MOVE W02-PDCHEQ	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
008450         PERFORM LOCKED-RECORD
008460	       GO TO READ-PDCHEQ.
008470	     MOVE 44		 TO WS-F-ERROR.
008480       PERFORM READ-ERROR.
	     GO TO READ-PDCHEQ-EXIT.

008370 READ-PDCHEQ-NEXT.
008380	     READ PDCHEQ NEXT.
008390     IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
008400	       GO TO READ-PDCHEQ-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
008420	       MOVE 44		 TO WS-F-ERROR
008400	       GO TO READ-PDCHEQ-EXIT.
008410     IF WS-STAT1 = "2" OR "3" OR "4"
008420	       MOVE 44		 TO WS-F-ERROR
008430         PERFORM READ-ERROR.
008440     IF RECORD-LOCKED
	       MOVE W02-PDCHEQ	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
008450         PERFORM LOCKED-RECORD
008460	       GO TO READ-PDCHEQ-NEXT.
008470	     MOVE 44		 TO WS-F-ERROR.
008480       PERFORM READ-ERROR.
	     GO TO READ-PDCHEQ-EXIT.

008370 READ-PDCHEQ-PREV.
008380	     READ PDCHEQ PREVIOUS.
008390     IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
008400	       GO TO READ-PDCHEQ-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
008420	       MOVE 44		 TO WS-F-ERROR
008400	       GO TO READ-PDCHEQ-EXIT.
008410     IF WS-STAT1 = "2" OR "3" OR "4"
008420	       MOVE 44		 TO WS-F-ERROR
008430         PERFORM READ-ERROR.
008440     IF RECORD-LOCKED
	       MOVE W02-PDCHEQ	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
008450         PERFORM LOCKED-RECORD
008460	       GO TO READ-PDCHEQ-PREV.
008470	     MOVE 44		 TO WS-F-ERROR.
008480       PERFORM READ-ERROR.
	     GO TO READ-PDCHEQ-EXIT.

008370 START-AT-PDCHEQ-KEY.
008380	     START PDCHEQ
	       KEY >= PDT-KEY.
008390     IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
008400	       GO TO READ-PDCHEQ-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
008420	       MOVE 44		 TO WS-F-ERROR
008400	       GO TO READ-PDCHEQ-EXIT.
008410     IF WS-STAT1 = "2" OR "3" OR "4"
008420	       MOVE 44		 TO WS-F-ERROR
008430         PERFORM READ-ERROR.
	     GO TO READ-PDCHEQ-EXIT.

008370 START-AT-PDCHEQ-AC.
008380	     START PDCHEQ
	       KEY >= PDT-AC.
008390     IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
008400	       GO TO READ-PDCHEQ-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
008420	       MOVE 44		 TO WS-F-ERROR
008400	       GO TO READ-PDCHEQ-EXIT.
008410     IF WS-STAT1 = "2" OR "3" OR "4"
008420	       MOVE 44		 TO WS-F-ERROR
008430         PERFORM READ-ERROR.

008370 START-AT-PDCHEQ-REF.
008380	     START PDCHEQ
	       KEY >= PDT-REFER.
008390     IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
008400	       GO TO READ-PDCHEQ-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
008420	       MOVE 44		 TO WS-F-ERROR
008400	       GO TO READ-PDCHEQ-EXIT.
008410     IF WS-STAT1 = "2" OR "3" OR "4"
008420	       MOVE 44		 TO WS-F-ERROR
008430         PERFORM READ-ERROR.

008490 READ-PDCHEQ-EXIT.
008500       EXIT.
