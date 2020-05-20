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
006840 READ-VEHICLE.
006850	     READ VEHICLE	 WITH IGNORE LOCK
	       KEY IS VEH-CODE.
006860     IF WS-STATUS = "00"
006890         MOVE ZERO         TO WS-F-ERROR
006870	       GO TO READ-VEHICLE-EXIT.
009900     IF WS-STATUS = "23"
006890	       MOVE 54		 TO WS-F-ERROR
006870	       GO TO READ-VEHICLE-EXIT.
006880     IF WS-STAT1 = "2" OR "3" OR "4"
006890	       MOVE 54		 TO WS-F-ERROR
006900         PERFORM READ-ERROR.
	     GO TO READ-VEHICLE-EXIT.

006840 READ-VEHICLE-LOCK.
006850	     READ VEHICLE WITH KEPT LOCK
	       KEY IS VEH-CODE.
006860     IF WS-STATUS = "00"
006890         MOVE ZERO         TO WS-F-ERROR
006870	       GO TO READ-VEHICLE-EXIT.
009900     IF WS-STATUS = "23"
006890	       MOVE 54		 TO WS-F-ERROR
006870	       GO TO READ-VEHICLE-EXIT.
006880     IF WS-STAT1 = "2" OR "3" OR "4"
006890	       MOVE 54		 TO WS-F-ERROR
006900         PERFORM READ-ERROR.
006910     IF RECORD-LOCKED
	       MOVE W02-VEHICLE	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
006920         PERFORM LOCKED-RECORD
006930	       GO TO READ-VEHICLE-LOCK.
008420     IF TOO-MANY-LOCKS
008430	       UNLOCK VEHICLE
008440	       GO TO READ-VEHICLE-LOCK.
	     GO TO READ-VEHICLE-EXIT.

006840 READ-VEHICLE-NEXT.
006850	     READ VEHICLE NEXT	 WITH IGNORE LOCK.
006860     IF WS-STATUS = "00"
006890         MOVE ZERO         TO WS-F-ERROR
006870	       GO TO READ-VEHICLE-EXIT.
009900     IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
006890	       MOVE 54		 TO WS-F-ERROR
006870	       GO TO READ-VEHICLE-EXIT.
006880     IF WS-STAT1 = "2" OR "3" OR "4"
006890	       MOVE 54		 TO WS-F-ERROR
006900         PERFORM READ-ERROR.
	     GO TO READ-VEHICLE-EXIT.

006840 READ-VEHICLE-NEXT-LOCK.
006850	     READ VEHICLE NEXT WITH KEPT LOCK.
006860     IF WS-STATUS = "00"
006890         MOVE ZERO         TO WS-F-ERROR
006870	       GO TO READ-VEHICLE-EXIT.
009900     IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
006890	       MOVE 54		 TO WS-F-ERROR
006870	       GO TO READ-VEHICLE-EXIT.
006880     IF WS-STAT1 = "2" OR "3" OR "4"
006890	       MOVE 54		 TO WS-F-ERROR
006900         PERFORM READ-ERROR.
006910     IF RECORD-LOCKED
	       MOVE W02-VEHICLE	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
006920         PERFORM LOCKED-RECORD
006930	       GO TO READ-VEHICLE-NEXT-LOCK.
008420     IF TOO-MANY-LOCKS
008430	       UNLOCK VEHICLE
008440	       GO TO READ-VEHICLE-NEXT-LOCK.
	     GO TO READ-VEHICLE-EXIT.

006840 START-AT-REGIST.
006850	     START VEHICLE
	       KEY >= VEH-CODE.
006860     IF WS-STATUS = "00"
006890         MOVE ZERO         TO WS-F-ERROR
006870	       GO TO READ-VEHICLE-EXIT.
009900     IF WS-STATUS = "23"
006890	       MOVE 54		 TO WS-F-ERROR
006870	       GO TO READ-VEHICLE-EXIT.
006880     IF WS-STAT1 = "2" OR "3" OR "4"
006890	       MOVE 54		 TO WS-F-ERROR
006900         PERFORM READ-ERROR.

007120 READ-VEHICLE-EXIT.
007130       EXIT.
