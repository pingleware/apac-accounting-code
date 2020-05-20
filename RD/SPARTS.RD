      *
      *     ***  ****    *   ****  *****  ***     ***** ***** *     *****
      *    *   * *   *  * *  *   *   *   *   *    *       *   *     *
      *    *     *   * *   * *   *   *   *        *       *   *     *
      *     ***  ****  ***** ****    *    ***     ***     *   *     ***
      *        * *     *   * *   *   *       *    *       *   *     *
      *    *   * *     *   * *   *   *   *   *    *       *   *     *
      *     ***  *     *   * *   *   *    ***     *     ***** ***** *****
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
009250 READ-SPARTS.
009260       READ SPARTS         WITH IGNORE LOCK
               KEY IS PRT-ITEM.
009270     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
009280         GO TO READ-SPARTS-EXIT.
009270     IF WS-STATUS = "23"
               MOVE 21           TO WS-F-ERROR
009280         GO TO READ-SPARTS-EXIT.
009290     IF WS-STAT1 = "2" OR "3" OR "4"
009300         MOVE 21           TO WS-F-ERROR
009310         PERFORM READ-ERROR.
009370       GO TO READ-SPARTS-EXIT.
      
009380 READ-SPARTS-LOCK.
009390       READ SPARTS WITH KEPT LOCK
               KEY IS PRT-ITEM.
009400     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
009280         GO TO READ-SPARTS-EXIT.
009270     IF WS-STATUS = "23"
               MOVE 21           TO WS-F-ERROR
009410         GO TO READ-SPARTS-EXIT.
009450     IF WS-STAT1 = "2" OR "3" OR "4"
009460         MOVE 21           TO WS-F-ERROR
009470         PERFORM READ-ERROR.
009480     IF RECORD-LOCKED
	       MOVE W02-SPARTS	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
009490         PERFORM LOCKED-RECORD
009500         GO TO READ-SPARTS-LOCK.
008420     IF TOO-MANY-LOCKS
008430         UNLOCK SPARTS
008440         GO TO READ-SPARTS-LOCK.
             GO TO READ-SPARTS-EXIT.

008190 READ-SPARTS-NEXT.
008200       READ SPARTS NEXT    WITH IGNORE LOCK.
008210     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
008220         GO TO READ-SPARTS-EXIT.
009900     IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
009910         MOVE 21           TO WS-F-ERROR
009920         GO TO READ-SPARTS-EXIT.
008230     IF WS-STAT1 = "2" OR "3" OR "4"
008240         MOVE 21           TO WS-F-ERROR
008250         PERFORM READ-ERROR.
008310       GO TO READ-SPARTS-EXIT.

008190 START-AT-PART-CODE.
008200       START SPARTS 
               KEY >= PRT-ITEM.
008210     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
008220         GO TO READ-SPARTS-EXIT.
009900     IF WS-STATUS = "23"
009910         MOVE 21           TO WS-F-ERROR
009920         GO TO READ-SPARTS-EXIT.
008230     IF WS-STAT1 = "2" OR "3" OR "4"
008240         MOVE 21           TO WS-F-ERROR
008250         PERFORM READ-ERROR.
      
009530 READ-SPARTS-EXIT.
009540       EXIT.
