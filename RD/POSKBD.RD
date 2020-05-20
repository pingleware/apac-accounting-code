      *
      *   ****	 ***   ***  *	* ****	****	 ***** *** *	 *****
      *   *   * *   * *   * *  *  *   * *   *	 *	*  *	 *
      *   *   * *   * *     * *   *   *	*   *	 *	*  *	 *
      *   ****	*   *  ***  **	  ****	*   *	 ***	*  *	 ***
      *   *	*   *	  * * *	  *   *	*   *	 *	*  *	 *
      *   *	*   * *   * *  *  *   *	*   *	 *	*  *	 *
      *   *	 ***   ***  *	* ****	****	 *     *** ***** *****
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
006080 READ-POSKBD.
	     READ POSKBD WITH IGNORE LOCK
		  KEY IS POS-KEY.
006100	   IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110	       GO TO READ-POSKBD-EXIT.
           IF WS-STATUS = "23"
	       MOVE 48		 TO WS-F-ERROR
	       GO TO READ-POSKBD-EXIT.
006120     IF WS-STAT1 = "2" OR "3" OR "4"
006130         MOVE 3            TO WS-F-ERROR
006140         PERFORM READ-ERROR.
	     GO TO READ-POSKBD-EXIT.

006210 READ-POSKBD-LOCK.
006220	     READ POSKBD WITH KEPT LOCK
		  KEY IS POS-KEY.
006230     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006240	       GO TO READ-POSKBD-EXIT.
           IF WS-STATUS = "23"
	       MOVE 48		 TO WS-F-ERROR
	       GO TO READ-POSKBD-EXIT.
006250     IF WS-STAT1 = "2" OR "3" OR "4"
006260	       MOVE 48		 TO WS-F-ERROR
006270         PERFORM READ-ERROR.
006280     IF RECORD-LOCKED
006290         PERFORM LOCKED-RECORD
006300	       GO TO READ-POSKBD-LOCK.
006310     IF TOO-MANY-LOCKS
006320	       UNLOCK POSKBD
006330	       GO TO READ-POSKBD-LOCK.
	     GO TO READ-POSKBD-EXIT.

000310 READ-POSKBD-NEXT.
000320	     READ POSKBD NEXT WITH IGNORE LOCK.
000330     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000340	       GO TO READ-POSKBD-EXIT.
000350     IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
000360	       MOVE 48		 TO WS-F-ERROR
000370	       GO TO READ-POSKBD-EXIT.
000380     IF WS-STAT1 = "2" OR "3" OR "4"
000390	       MOVE 48		 TO WS-F-ERROR
000400         PERFORM READ-ERROR.
	     GO TO READ-POSKBD-EXIT.

000020 START-AT-POS-KEY.
000030	     START POSKBD
	       KEY >= POS-KEY.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-POSKBD-EXIT.
           IF WS-STATUS = "23"
	       MOVE 48		 TO WS-F-ERROR
000050	       GO TO READ-POSKBD-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 48		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
	     GO TO READ-POSKBD-EXIT.

000020 START-AT-POS-PLU.
000030	     START POSKBD
	       KEY >= POS-KEY2.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-POSKBD-EXIT.
           IF WS-STATUS = "23"
	       MOVE 48		 TO WS-F-ERROR
000050	       GO TO READ-POSKBD-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 48		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.

006520 READ-POSKBD-EXIT.
006530       EXIT.
