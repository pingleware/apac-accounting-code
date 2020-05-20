      *
      *    ****  ***** ****  ***** ***** *   *	  ***** *** *	  *****
      *    *   * *     *   *   *   *	 *   *	  *	 *  *	  *
      *    *   * *     *   *   *   *	  * *	  *	 *  *	  *
      *    *   * ***   ****    *   ***	   *	  ***	 *  *	  ***
      *    *   * *     *   *   *   *	  * *	  *	 *  *	  *
      *    *   * *     *   *   *   *	 *   *	  *	 *  *	  *
      *    ****  ***** ****    *   ***** *   *	  *	*** ***** *****
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
000020 READ-DEBTEX.
000030	     READ DEBTEX WITH IGNORE LOCK
	       KEY IS DXT-KEY.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-DEBTEX-EXIT.
           IF WS-STATUS = "23"
	       MOVE 55		 TO WS-F-ERROR
000050	       GO TO READ-DEBTEX-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 55		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
	     GO TO READ-DEBTEX-EXIT.

000150 READ-DEBTEX-LOCK.
000160	     READ DEBTEX WITH KEPT LOCK
	       KEY IS DXT-KEY.
000170     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000180	       GO TO READ-DEBTEX-EXIT.
           IF WS-STATUS = "23"
	       MOVE 55		 TO WS-F-ERROR
000050	       GO TO READ-DEBTEX-EXIT.
000190     IF WS-STAT1 = "2" OR "3" OR "4"
000200	       MOVE 55		 TO WS-F-ERROR
000210         PERFORM READ-ERROR.
000220     IF RECORD-LOCKED
	       MOVE W02-DEBTEX	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000230         PERFORM LOCKED-RECORD
000240	       GO TO READ-DEBTEX-LOCK.
000250     IF TOO-MANY-LOCKS
000260	       UNLOCK DEBTEX
000270	       GO TO READ-DEBTEX-LOCK.
	     GO TO READ-DEBTEX-EXIT.

000310 READ-DEBTEX-NEXT.
000320	     READ DEBTEX NEXT WITH IGNORE LOCK.
000330     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000340	       GO TO READ-DEBTEX-EXIT.
000350     IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
000360	       MOVE 55		 TO WS-F-ERROR
000370	       GO TO READ-DEBTEX-EXIT.
000380     IF WS-STAT1 = "2" OR "3" OR "4"
000390	       MOVE 55		 TO WS-F-ERROR
000400         PERFORM READ-ERROR.
	     GO TO READ-DEBTEX-EXIT.

000310 READ-DEBTEX-NEXT-LOCK.
000320	     READ DEBTEX NEXT WITH KEPT LOCK.
000330     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000340	       GO TO READ-DEBTEX-EXIT.
000350     IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
000360	       MOVE 55		 TO WS-F-ERROR
000370	       GO TO READ-DEBTEX-EXIT.
000380     IF WS-STAT1 = "2" OR "3" OR "4"
000390	       MOVE 55		 TO WS-F-ERROR
000400         PERFORM READ-ERROR.
000410     IF RECORD-LOCKED
	       MOVE W02-DEBTEX	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000420	       PERFORM LOCKED-RECORD
000430	       GO TO READ-DEBTEX-NEXT-LOCK.
000250     IF TOO-MANY-LOCKS
000260	       UNLOCK DEBTEX
000270	       GO TO READ-DEBTEX-NEXT-LOCK.
	     GO TO READ-DEBTEX-EXIT.

000310 READ-DEBTEX-PREV.
000320	     READ DEBTEX PREVIOUS WITH IGNORE LOCK.
000330     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000340	       GO TO READ-DEBTEX-EXIT.
000350     IF (WS-STATUS = "23") OR
	      (WS-STAT1 = "1" OR "4")
000360	       MOVE 55		 TO WS-F-ERROR
000370	       GO TO READ-DEBTEX-EXIT.
000380	   IF WS-STAT1 = "2" OR "3"
000390	       MOVE 55		 TO WS-F-ERROR
000400         PERFORM READ-ERROR.
	     GO TO READ-DEBTEX-EXIT.

000020 START-AT-DBEX-AC.
000030	     START DEBTEX
	       KEY >= DXT-KEY.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-DEBTEX-EXIT.
           IF WS-STATUS = "23"
	       MOVE 55		 TO WS-F-ERROR
000050	       GO TO READ-DEBTEX-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 55		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
	     GO TO READ-DEBTEX-EXIT.

000020 START-AT-DBEX-AKEY.
000030	     START DEBTEX
	       KEY >= DXT-AKEY.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-DEBTEX-EXIT.
           IF WS-STATUS = "23"
	       MOVE 55		 TO WS-F-ERROR
000050	       GO TO READ-DEBTEX-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 55		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
	     GO TO READ-DEBTEX-EXIT.

000020 START-AT-DBEX-SKEY.
000030	     START DEBTEX
	       KEY >= DXT-SKEY.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-DEBTEX-EXIT.
           IF WS-STATUS = "23"
	       MOVE 55		 TO WS-F-ERROR
000050	       GO TO READ-DEBTEX-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 55		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.

000470 READ-DEBTEX-EXIT.
000480       EXIT.
