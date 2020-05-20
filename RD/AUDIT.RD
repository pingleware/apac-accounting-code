      *
      *     ***  *   * ****  ***** *****    ***** *** *     *****
      *    *   * *   * *   *   *     *	    *	   *  *     *
      *    *   * *   * *   *   *     *	    *	   *  *     *
      *    ***** *   * *   *   *     *	    ***    *  *     ***
      *    *   * *   * *   *   *     *	    *	   *  *     *
      *    *   * *   * *   *   *     *	    *	   *  *     *
      *    *   *  ***  ****  *****   *	    *	  *** ***** *****
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

008460 READ-AUDIT.
008470	     READ AUDIT 	 WITH IGNORE LOCK.
008480     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
008490         GO TO READ-AUDIT-EXIT.
000080	     PERFORM READ-ERROR.
008580	     GO TO READ-AUDIT-END.

008590 READ-AUDIT-LOCK.
008600       READ AUDIT WITH KEPT LOCK.
008610     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
008620         GO TO READ-AUDIT-EXIT.
000080	     PERFORM READ-ERROR.
000220	   IF WS-ERROR = 1
	       MOVE W02-AUDITF	 TO WS-FILE
	       MOVE WS-AUDKEY	 TO WS-KEY
008670         PERFORM LOCKED-RECORD
008680         GO TO READ-AUDIT-LOCK.
000250	   IF WS-ERROR = 8
008700	       UNLOCK AUDIT
008710         GO TO READ-AUDIT-LOCK.

000470 READ-AUDIT-END.
	   IF NOT (WS-ERROR = ZERO)
	       STOP RUN.

       READ-AUDIT-RESTORE.
	     PERFORM RESTORE-SCREEN.
	     MOVE "N"		 TO WS-SKIP.

008740 READ-AUDIT-EXIT.
008750       EXIT.
