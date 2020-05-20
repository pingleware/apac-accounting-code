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
006840 READ-JOBWAR.
006850	     READ JOBWAR         WITH IGNORE LOCK
	       KEY IS JBW-KEY.
006860     IF WS-STATUS = "00"
006890         MOVE ZERO         TO WS-F-ERROR
006870	       GO TO READ-JOBWAR-EXIT.
009900     IF WS-STATUS = "23"
006890	       MOVE 28		 TO WS-F-ERROR
006870	       GO TO READ-JOBWAR-EXIT.
006880     IF WS-STAT1 = "2" OR "3" OR "4"
006890	       MOVE 28		 TO WS-F-ERROR
006900         PERFORM READ-ERROR.
006910*    IF RECORD-LOCKED
006920*        PERFORM LOCKED-RECORD
006930*	       GO TO READ-JOBWAR.
	     GO TO READ-JOBWAR-EXIT.

006840 READ-JOBWAR-LOCK.
006850	     READ JOBWAR WITH KEPT LOCK
	       KEY IS JBW-KEY.
006860     IF WS-STATUS = "00"
006890         MOVE ZERO         TO WS-F-ERROR
006870	       GO TO READ-JOBWAR-EXIT.
009900     IF WS-STATUS = "23"
006890	       MOVE 28		 TO WS-F-ERROR
006870	       GO TO READ-JOBWAR-EXIT.
006880     IF WS-STAT1 = "2" OR "3" OR "4"
006890	       MOVE 28		 TO WS-F-ERROR
006900         PERFORM READ-ERROR.
006910     IF RECORD-LOCKED
	       MOVE W02-JOBWAR	 TO WS-FILE
006920         PERFORM LOCKED-RECORD
006930	       GO TO READ-JOBWAR-LOCK.
008420     IF TOO-MANY-LOCKS
008430	       UNLOCK JOBWAR
008440	       GO TO READ-JOBWAR-LOCK.
	     GO TO READ-JOBWAR-EXIT.

006840 READ-JOBWAR-NEXT.
006850	     READ JOBWAR NEXT    WITH IGNORE LOCK.
006860     IF WS-STATUS = "00"
006890         MOVE ZERO         TO WS-F-ERROR
006870	       GO TO READ-JOBWAR-EXIT.
009900     IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
006890	       MOVE 28		 TO WS-F-ERROR
006870	       GO TO READ-JOBWAR-EXIT.
006880     IF WS-STAT1 = "2" OR "3" OR "4"
006890	       MOVE 28		 TO WS-F-ERROR
006900         PERFORM READ-ERROR.
006910*    IF RECORD-LOCKED
006920*        PERFORM LOCKED-RECORD
006930*	       GO TO READ-JOBWAR-NEXT.
	     GO TO READ-JOBWAR-EXIT.

006840 READ-JOBWAR-NEXT-LOCK.
006850	     READ JOBWAR NEXT    WITH KEPT LOCK.
006860     IF WS-STATUS = "00"
006890         MOVE ZERO         TO WS-F-ERROR
006870	       GO TO READ-JOBWAR-EXIT.
009900     IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
006890	       MOVE 28		 TO WS-F-ERROR
006870	       GO TO READ-JOBWAR-EXIT.
006880     IF WS-STAT1 = "2" OR "3" OR "4"
006890	       MOVE 28		 TO WS-F-ERROR
006900         PERFORM READ-ERROR.
006910     IF RECORD-LOCKED
	       MOVE W02-JOBWAR	 TO WS-FILE
006920         PERFORM LOCKED-RECORD
006930	       GO TO READ-JOBWAR-NEXT-LOCK.
008420     IF TOO-MANY-LOCKS
008430	       UNLOCK JOBWAR
008440	       GO TO READ-JOBWAR-NEXT-LOCK.
	     GO TO READ-JOBWAR-EXIT.

006840 READ-JOBWAR-PREV.
006850	     READ JOBWAR PREVIOUS
                                 WITH IGNORE LOCK.
006860     IF WS-STATUS = "00"
006890         MOVE ZERO         TO WS-F-ERROR
006870	       GO TO READ-JOBWAR-EXIT.
009900     IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
006890	       MOVE 28		 TO WS-F-ERROR
006870	       GO TO READ-JOBWAR-EXIT.
006880     IF WS-STAT1 = "2" OR "3" OR "4"
006890	       MOVE 28		 TO WS-F-ERROR
006900         PERFORM READ-ERROR.
006910*    IF RECORD-LOCKED
006920*        PERFORM LOCKED-RECORD
006930*        GO TO READ-JOBWAR-PREV.
	     GO TO READ-JOBWAR-EXIT.

006840 START-AT-JBW-KEY.
006850	     START JOBWAR
	       KEY >= JBW-KEY.
006860     IF WS-STATUS = "00"
006890         MOVE ZERO         TO WS-F-ERROR
006870	       GO TO READ-JOBWAR-EXIT.
009900     IF WS-STATUS = "23"
006890	       MOVE 28		 TO WS-F-ERROR
006870	       GO TO READ-JOBWAR-EXIT.
006880     IF WS-STAT1 = "2" OR "3" OR "4"
006890	       MOVE 28		 TO WS-F-ERROR
006900         PERFORM READ-ERROR.

007120 READ-JOBWAR-EXIT.
007130       EXIT.
