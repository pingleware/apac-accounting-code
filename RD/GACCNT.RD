      *
      *     ***        *     ***** ****   ***	  ***** *** *	  *****
      *    *   *       *     *	   *   * *   *	  *	 *  *	  *
      *    *	       *     *	   *   * *	  *	 *  *	  *
      *    *	  **   *     ****  *   * *	  ***	 *  *	  ***
      *    *  **       *     *	   *   * *  **	  *	 *  *	  *
      *    *   *       *     *	   *   * *   *	  *	 *  *	  *
      *     ***        ***** ***** ****   ***	  *	*** ***** *****
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
007910 READ-GACCNT.
007920       READ GACCNT
               KEY IS G-AC.
007930     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
007940         GO TO READ-GACCNT-EXIT.
           IF WS-STATUS = "23"
               MOVE 8            TO WS-F-ERROR
007940         GO TO READ-GACCNT-EXIT.
007950     IF WS-STAT1 = "2" OR "3" OR "4"
007960         MOVE 8            TO WS-F-ERROR
007970         PERFORM READ-ERROR.
007980     IF RECORD-LOCKED
007990         PERFORM LOCKED-RECORD
008000         GO TO READ-GACCNT.
008030       GO TO READ-GACCNT-EXIT.

008040 READ-GACCNT-LOCK.
008050       READ GACCNT WITH KEPT LOCK
               KEY IS G-AC.
008060     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
008070         GO TO READ-GACCNT-EXIT.
           IF WS-STATUS = "23"
               MOVE 8            TO WS-F-ERROR
007940         GO TO READ-GACCNT-EXIT.
008080     IF WS-STAT1 = "2" OR "3" OR "4"
008090         MOVE 8            TO WS-F-ERROR
008100         PERFORM READ-ERROR.
008110     IF RECORD-LOCKED
008120         PERFORM LOCKED-RECORD
008130         GO TO READ-GACCNT-LOCK.
008140     IF TOO-MANY-LOCKS
008150         UNLOCK GACCNT
008160         GO TO READ-GACCNT-LOCK.
008190       GO TO READ-GACCNT-EXIT.

007910 READ-GACCNT-NEXT.
007920       READ GACCNT NEXT.
007930     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
007940         GO TO READ-GACCNT-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
               MOVE 8            TO WS-F-ERROR
007940         GO TO READ-GACCNT-EXIT.
007950     IF WS-STAT1 = "2" OR "3" OR "4"
007960         MOVE 8            TO WS-F-ERROR
007970         PERFORM READ-ERROR.
007980     IF RECORD-LOCKED
007990         PERFORM LOCKED-RECORD
008000         GO TO READ-GACCNT-NEXT.
008140     IF TOO-MANY-LOCKS
008150         UNLOCK GACCNT
008160	       GO TO READ-GACCNT-NEXT.
008030       GO TO READ-GACCNT-EXIT.

007910 READ-GACCNT-NEXT-LOCK.
007920       READ GACCNT NEXT WITH KEPT LOCK.
007930     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
007940         GO TO READ-GACCNT-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
               MOVE 8            TO WS-F-ERROR
007940         GO TO READ-GACCNT-EXIT.
007950     IF WS-STAT1 = "2" OR "3" OR "4"
007960         MOVE 8            TO WS-F-ERROR
007970         PERFORM READ-ERROR.
007980     IF RECORD-LOCKED
007990         PERFORM LOCKED-RECORD
008000         GO TO READ-GACCNT-NEXT-LOCK.
008140     IF TOO-MANY-LOCKS
008150         UNLOCK GACCNT
008160         GO TO READ-GACCNT-NEXT-LOCK.
008030       GO TO READ-GACCNT-EXIT.

006080 READ-GACCNT-PREV.
006090	     READ GACCNT PREVIOUS WITH IGNORE LOCK.
006100	   IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
006110	       GO TO READ-GACCNT-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
	       MOVE 8		 TO WS-F-ERROR
	       GO TO READ-GACCNT-EXIT.
006120     IF WS-STAT1 = "2" OR "3" OR "4"
006130	       MOVE 8		 TO WS-F-ERROR
006140         PERFORM READ-ERROR.
006200	     GO TO READ-GACCNT-EXIT.

       START-AT-LEDG-AC.
	     START GACCNT
	       KEY >= G-AC.
007930	   IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
007940         GO TO READ-GACCNT-EXIT.
	   IF WS-STATUS = "23"
               MOVE 8            TO WS-F-ERROR
007940         GO TO READ-GACCNT-EXIT.
007950	   IF WS-STAT1 = "2" OR "3" OR "4"
007960         MOVE 8            TO WS-F-ERROR
007970         PERFORM READ-ERROR.
008030       GO TO READ-GACCNT-EXIT.

       START-AT-LEDG-TYPE.
	     START GACCNT
	       KEY >= G-FSKEY.
007930	   IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
007940         GO TO READ-GACCNT-EXIT.
	   IF WS-STATUS = "23"
               MOVE 8            TO WS-F-ERROR
007940         GO TO READ-GACCNT-EXIT.
007950	   IF WS-STAT1 = "2" OR "3" OR "4"
007960         MOVE 8            TO WS-F-ERROR
007970         PERFORM READ-ERROR.

008350 READ-GACCNT-EXIT.
008360       EXIT.
