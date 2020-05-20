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
008460 READ-PRCLAB.
008470       READ PRCLAB.
008480     IF WS-STATUS = "00"
008490         GO TO READ-PRCLAB-EXIT.
008500     IF WS-STAT1 = "2" OR "3" OR "4"
008510         MOVE 37           TO WS-F-ERROR
008520         PERFORM READ-ERROR.
008530     IF RECORD-LOCKED
008540         PERFORM LOCKED-RECORD
008550         GO TO READ-PRCLAB.
008560       MOVE 37             TO WS-F-ERROR.
008570       PERFORM READ-ERROR.
008580       GO TO READ-PRCLAB-EXIT.

008590 READ-PRCLAB-LOCK.
008600       READ PRCLAB WITH KEPT LOCK.
008610     IF WS-STATUS = "00"
008620         GO TO READ-PRCLAB-EXIT.
008630     IF WS-STAT1 = "2" OR "3" OR "4"
008640         MOVE 37           TO WS-F-ERROR
008650         PERFORM READ-ERROR.
008660     IF RECORD-LOCKED
008670         PERFORM LOCKED-RECORD
008680         GO TO READ-PRCLAB-LOCK.
008690     IF TOO-MANY-LOCKS
008700         UNLOCK PRCLAB
008710         GO TO READ-PRCLAB-LOCK.
008720       MOVE 37             TO WS-F-ERROR.
008730       PERFORM READ-ERROR.

008740 READ-PRCLAB-EXIT.
008750       EXIT.
