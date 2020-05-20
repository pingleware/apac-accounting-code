      *
      *    ****  ***** ****  *****  ***  *****	  ***** *** *	  *****
      *    *   * *     *   *   *   *   *   *	  *	 *  *	  *
      *    *   * *     *   *   *   *   *   *	  *	 *  *	  *
      *    *   * ***   ****    *   *   *   *	  ***	 *  *	  ***
      *    *   * *     *   *   *   *   *   *	  *	 *  *	  *
      *    *   * *     *   *   *   *   *   *	  *	 *  *	  *
      *    ****  ***** ****    *    ***    *	  *	*** ***** *****
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
000020 READ-DEBTOT.
000030	     READ DEBTOT WITH IGNORE LOCK
	       KEY IS TOT-KEY.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-DEBTOT-EXIT.
           IF WS-STATUS = "23"
	       MOVE 65		 TO WS-F-ERROR
000050	       GO TO READ-DEBTOT-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 65		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
	     GO TO READ-DEBTOT-EXIT.

000150 READ-DEBTOT-LOCK.
000160	     READ DEBTOT WITH KEPT LOCK
	       KEY IS TOT-KEY.
000170     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000180	       GO TO READ-DEBTOT-EXIT.
           IF WS-STATUS = "23"
	       MOVE 65		 TO WS-F-ERROR
000050	       GO TO READ-DEBTOT-EXIT.
000190     IF WS-STAT1 = "2" OR "3" OR "4"
000200	       MOVE 65		 TO WS-F-ERROR
000210         PERFORM READ-ERROR.
000220     IF RECORD-LOCKED
	       MOVE W02-DEBTOT	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000230         PERFORM LOCKED-RECORD
000240	       GO TO READ-DEBTOT-LOCK.
000250     IF TOO-MANY-LOCKS
000260	       UNLOCK DEBTOT
000270	       GO TO READ-DEBTOT-LOCK.
	     GO TO READ-DEBTOT-EXIT.

000310 READ-DEBTOT-NEXT.
000320	     READ DEBTOT NEXT WITH IGNORE LOCK.
000330     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000340	       GO TO READ-DEBTOT-EXIT.
000350     IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
000360	       MOVE 65		 TO WS-F-ERROR
000370	       GO TO READ-DEBTOT-EXIT.
000380     IF WS-STAT1 = "2" OR "3" OR "4"
000390	       MOVE 65		 TO WS-F-ERROR
000400         PERFORM READ-ERROR.
	     GO TO READ-DEBTOT-EXIT.

000310 READ-DEBTOT-NEXT-LOCK.
000320	     READ DEBTOT NEXT WITH KEPT LOCK.
000330     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000340	       GO TO READ-DEBTOT-EXIT.
000350     IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
000360	       MOVE 65		 TO WS-F-ERROR
000370	       GO TO READ-DEBTOT-EXIT.
000380     IF WS-STAT1 = "2" OR "3" OR "4"
000390	       MOVE 65		 TO WS-F-ERROR
000400         PERFORM READ-ERROR.
000410     IF RECORD-LOCKED
	       MOVE W02-DEBTOT	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000420	       PERFORM LOCKED-RECORD
000430	       GO TO READ-DEBTOT-NEXT-LOCK.
000250     IF TOO-MANY-LOCKS
000260	       UNLOCK DEBTOT
000270	       GO TO READ-DEBTOT-NEXT-LOCK.
	     GO TO READ-DEBTOT-EXIT.

000310 READ-DEBTOT-PREV.
000320	     READ DEBTOT PREVIOUS WITH IGNORE LOCK.
000330     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000340	       GO TO READ-DEBTOT-EXIT.
000350     IF (WS-STATUS = "23") OR
	      (WS-STAT1 = "1" OR "4")
000360	       MOVE 65		 TO WS-F-ERROR
000370	       GO TO READ-DEBTOT-EXIT.
000380	   IF WS-STAT1 = "2" OR "3"
000390	       MOVE 65		 TO WS-F-ERROR
000400         PERFORM READ-ERROR.
	     GO TO READ-DEBTOT-EXIT.

000020 START-AT-DBEX-AC.
000030	     START DEBTOT
	       KEY >= TOT-KEY.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-DEBTOT-EXIT.
           IF WS-STATUS = "23"
	       MOVE 65		 TO WS-F-ERROR
000050	       GO TO READ-DEBTOT-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 65		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
	     GO TO READ-DEBTOT-EXIT.

000020 START-AT-DBEX-AKEY.
000030	     START DEBTOT
	       KEY >= TOT-AKEY.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-DEBTOT-EXIT.
           IF WS-STATUS = "23"
	       MOVE 65		 TO WS-F-ERROR
000050	       GO TO READ-DEBTOT-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 65		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.

000470 READ-DEBTOT-EXIT.
000480       EXIT.
