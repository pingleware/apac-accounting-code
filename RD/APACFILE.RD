      *
      *     ***  ****	***   ***  ***** ****	  ***** *** *	  *****
      *    *   * *   * *   * *	 *   *	 *   *	  *	 *  *	  *
      *    *   * *   * *   * *	     *	 *   *	  *	 *  *	  *
      *    ***** ****  ***** *	     *	 *   *	  ***	 *  *	  ***
      *    *   * *     *   * *	     *	 *   *	  *	 *  *	  *
      *    *   * *     *   * *	 *   *	 *   *	  *	 *  *	  *
      *    *   * *     *   *  ***  ***** ****	  *	*** ***** *****
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
000020 READ-APACFILE.
000030	     READ APACFILE WITH IGNORE LOCK
	       KEY IS APAC-KEY.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-APACFILE-EXIT.
	     MOVE 99		 TO WS-F-ERROR.
000080	     PERFORM READ-ERROR.
	     GO TO READ-APACFILE-END.

000150 READ-APACFILE-LOCK.
000160	     READ APACFILE WITH KEPT LOCK
	       KEY IS APAC-KEY.
000170     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000180	       GO TO READ-APACFILE-EXIT.
	     MOVE 99		 TO WS-F-ERROR.
000210	     PERFORM READ-ERROR.
000220	   IF WS-ERROR = 1
	       MOVE W02-APACFILE TO WS-FILE
000230         PERFORM LOCKED-RECORD
000240	       GO TO READ-APACFILE-LOCK.
000250	   IF WS-ERROR = 8
000260	       UNLOCK APACFILE
000270	       GO TO READ-APACFILE-LOCK.
	     GO TO READ-APACFILE-END.

000310 READ-APACFILE-NEXT.
000320	     READ APACFILE NEXT WITH IGNORE LOCK.
000330     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000340	       GO TO READ-APACFILE-EXIT.
	     MOVE 99		 TO WS-F-ERROR.
	     PERFORM READ-ERROR.
000350	   IF WS-ERROR = 3
000370	       GO TO READ-APACFILE-RESTORE.
	     GO TO READ-APACFILE-END.

000310 READ-APACFILE-NEXT-LOCK.
000320	     READ APACFILE NEXT WITH KEPT LOCK.
000330     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000340	       GO TO READ-APACFILE-EXIT.
	     MOVE 99		 TO WS-F-ERROR.
	     PERFORM READ-ERROR.
000350	   IF WS-ERROR = 3
000370	       GO TO READ-APACFILE-RESTORE.
000220	   IF WS-ERROR = 1
	       MOVE W02-APACFILE TO WS-FILE
000230         PERFORM LOCKED-RECORD
000240	       GO TO READ-APACFILE-NEXT-LOCK.
000250	   IF WS-ERROR = 8
000260	       UNLOCK APACFILE
000270	       GO TO READ-APACFILE-NEXT-LOCK.
	     GO TO READ-APACFILE-END.

000310 READ-APACFILE-PREV.
000320	     READ APACFILE PREVIOUS WITH IGNORE LOCK.
000330     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000340	       GO TO READ-APACFILE-EXIT.
	     MOVE 99		 TO WS-F-ERROR.
	     PERFORM READ-ERROR.
000350	   IF WS-ERROR = 3
000370	       GO TO READ-APACFILE-RESTORE.
	     GO TO READ-APACFILE-END.

000020 START-AT-APAC-KEY.
000030	     START APACFILE
	       KEY >= APAC-KEY.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-APACFILE-EXIT.
	     MOVE 99		 TO WS-F-ERROR.
	     PERFORM READ-ERROR.
000350	   IF WS-ERROR = 3
000370	       GO TO READ-APACFILE-RESTORE.

000470 READ-APACFILE-END.
	   IF NOT (WS-ERROR = ZERO)
	       STOP RUN.

       READ-APACFILE-RESTORE.
	     PERFORM RESTORE-SCREEN.
	     MOVE "N"		 TO WS-SKIP.

000470 READ-APACFILE-EXIT.
000480       EXIT.
