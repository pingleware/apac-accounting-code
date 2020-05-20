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
009860 READ-ROUTE.
009870	     READ ROUTE WITH IGNORE LOCK
	       KEY IS RTE-ROUTE.
009880     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
009890         GO TO READ-ROUTE-EXIT.
           IF WS-STATUS = 23
               MOVE 30           TO WS-F-ERROR
               GO TO READ-ROUTE-EXIT.
009900     IF WS-STAT1 = "2" OR "3" OR "4"
009910         MOVE 30           TO WS-F-ERROR
009920         PERFORM READ-ERROR.
009980       GO TO READ-ROUTE-EXIT.
      
009990 READ-ROUTE-LOCK.
010000       READ ROUTE WITH KEPT LOCK
	       KEY IS RTE-ROUTE.
010010     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
010020         GO TO READ-ROUTE-EXIT.
           IF WS-STATUS = 23
               MOVE 30           TO WS-F-ERROR
               GO TO READ-ROUTE-EXIT.
010030     IF WS-STAT1 = "2" OR "3" OR "4"
010080         MOVE 30       TO WS-F-ERROR
010090         PERFORM READ-ERROR.
010100     IF RECORD-LOCKED
	       MOVE W02-ROUTE	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
010110	       PERFORM LOCKED-RECORD
010120         GO TO READ-ROUTE-LOCK.
008420     IF TOO-MANY-LOCKS
008430         UNLOCK ROUTE
008440         GO TO READ-ROUTE-LOCK.
009980       GO TO READ-ROUTE-EXIT.

008190 READ-ROUTE-NEXT.
008200	     READ ROUTE NEXT WITH IGNORE LOCK.
008210     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
008220         GO TO READ-ROUTE-EXIT.
009900     IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
009910         MOVE 30           TO WS-F-ERROR
009920         GO TO READ-ROUTE-EXIT.
008230     IF WS-STAT1 = "2" OR "3" OR "4"
008240         MOVE 30           TO WS-F-ERROR
008250         PERFORM READ-ERROR.
008310	     GO TO READ-ROUTE-EXIT.

008190 READ-ROUTE-NEXT-LOCK.
008200       READ ROUTE NEXT WITH KEPT LOCK.
008210     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
008220         GO TO READ-ROUTE-EXIT.
009900     IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
009910         MOVE 30           TO WS-F-ERROR
009920         GO TO READ-ROUTE-EXIT.
008230     IF WS-STAT1 = "2" OR "3" OR "4"
008240         MOVE 30           TO WS-F-ERROR
008250         PERFORM READ-ERROR.
008260     IF RECORD-LOCKED
	       MOVE W02-ROUTE	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
008270	       PERFORM LOCKED-RECORD
008280         GO TO READ-ROUTE-NEXT-LOCK.
008420     IF TOO-MANY-LOCKS
008430         UNLOCK ROUTE
008440         GO TO READ-ROUTE-NEXT-LOCK.
008310       GO TO READ-ROUTE-EXIT.

008190 READ-ROUTE-PREV.
008200	     READ ROUTE PREVIOUS.
008210     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
008220         GO TO READ-ROUTE-EXIT.
009900     IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
009910         MOVE 30           TO WS-F-ERROR
009920         GO TO READ-ROUTE-EXIT.
008230     IF WS-STAT1 = "2" OR "3" OR "4"
008240         MOVE 30           TO WS-F-ERROR
008250         PERFORM READ-ERROR.
008310	     GO TO READ-ROUTE-EXIT.

008190 START-AT-ROUTE.
008200       START ROUTE 
	       KEY >= RTE-ROUTE.
008210     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
008220         GO TO READ-ROUTE-EXIT.
009900     IF WS-STATUS = "23"
009910         MOVE 30           TO WS-F-ERROR
009920         GO TO READ-ROUTE-EXIT.
008230     IF WS-STAT1 = "2" OR "3" OR "4"
008240         MOVE 30           TO WS-F-ERROR
008250         PERFORM READ-ERROR.

010150 READ-ROUTE-EXIT.
010160       EXIT.
