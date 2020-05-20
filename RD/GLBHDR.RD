      *
      *     ***  *     ****  *	 * ****  ****	  ***** *** *	  *****
      *    *   * *     *   * *	 * *   * *   *	  *	 *  *	  *
      *    *	 *     *   * *	 * *   * *   *	  *	 *  *	  *
      *    *	 *     ****  ***** *   * ****	  ***	 *  *	  ***
      *    *  ** *     *   * *	 * *   * *   *	  *	 *  *	  *
      *    *   * *     *   * *	 * *   * *   *	  *	 *  *	  *
      *     ***  ***** ****  *	 * ****  *   *	  *	*** ***** *****
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
000020 READ-GLBHDR.
000030	     READ GLBHDR WITH IGNORE LOCK
	       KEY IS GBH-KEY.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-GLBHDR-EXIT.
000040     IF WS-STATUS = "23"
	       MOVE 51		 TO WS-F-ERROR
000050	       GO TO READ-GLBHDR-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 51		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
	     GO TO READ-GLBHDR-EXIT.

000020 READ-GLBHDR-LOCK.
000030	     READ GLBHDR WITH KEPT LOCK
	       KEY IS GBH-KEY.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-GLBHDR-EXIT.
000040     IF WS-STATUS = "23"
	       MOVE 51		 TO WS-F-ERROR
000050	       GO TO READ-GLBHDR-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 51		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
000090     IF RECORD-LOCKED
000070	       MOVE 51		 TO WS-F-ERROR.
	     GO TO READ-GLBHDR-EXIT.

000020 READ-GLBHDR-NEXT.
000030	     READ GLBHDR NEXT.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-GLBHDR-EXIT.
000040     IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
	       MOVE 51		 TO WS-F-ERROR
000050	       GO TO READ-GLBHDR-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 51		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
000090     IF RECORD-LOCKED
	       MOVE W02-GLBHDR	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000100         PERFORM LOCKED-RECORD
000110	       GO TO READ-GLBHDR-NEXT.
	     GO TO READ-GLBHDR-EXIT.

000020 READ-GLBHDR-PREV.
000030	     READ GLBHDR PREVIOUS.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-GLBHDR-EXIT.
000040     IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
	       MOVE 51		 TO WS-F-ERROR
000050	       GO TO READ-GLBHDR-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 51		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
000090     IF RECORD-LOCKED
	       MOVE W02-GLBHDR	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000100         PERFORM LOCKED-RECORD
000110	       GO TO READ-GLBHDR-PREV.
	     GO TO READ-GLBHDR-EXIT.

       START-AT-GLBHDR-KEY.
000030	     START GLBHDR
	       KEY >= GBH-KEY.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-GLBHDR-EXIT.
000040     IF WS-STATUS = "23"
	       MOVE 51		 TO WS-F-ERROR
000050	       GO TO READ-GLBHDR-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 51		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
	     GO TO READ-GLBHDR-EXIT.

000150 READ-GLBHDR-EXIT.
000160       EXIT.
