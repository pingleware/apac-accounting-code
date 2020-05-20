      *
      *     ***   ***	***  ***** ***** *****	  ***** *** *	  *****
      *    *   * *   * *   * *	     *	   *	  *	 *  *	  *
      *    *   * *     *     *	     *	   *	  *	 *  *	  *
      *    *****  ***	***  ***     *	   *	  ***	 *  *	  ***
      *    *   *     *	   * *	     *	   *	  *	 *  *	  *
      *    *   * *   * *   * *	     *	   *	  *	 *  *	  *
      *    *   *  ***	***  *****   *	   *	  *	*** ***** *****
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
000020 READ-ASSETT.
000030	     READ ASSETT WITH IGNORE LOCK
	       KEY IS ASS-CODE.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-ASSETT-EXIT.
000210	     PERFORM READ-ERROR.
	   IF WS-ERROR = 3
	       MOVE 50		 TO WS-F-ERROR
000050	       GO TO READ-ASSETT-RESTORE.
	     GO TO READ-ASSETT-END.

000150 READ-ASSETT-LOCK.
000160	     READ ASSETT WITH KEPT LOCK
	       KEY IS ASS-CODE.
000170     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000180	       GO TO READ-ASSETT-EXIT.
000210	     PERFORM READ-ERROR.
	   IF WS-ERROR = 3
	       MOVE 50		 TO WS-F-ERROR
000050	       GO TO READ-ASSETT-RESTORE.
000220	   IF WS-ERROR = 1
	       MOVE W02-ASSETT	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000230         PERFORM LOCKED-RECORD
000240	       GO TO READ-ASSETT-LOCK.
000250	   IF WS-ERROR = 8
000260	       UNLOCK ASSETT
000270	       GO TO READ-ASSETT-LOCK.
	     GO TO READ-ASSETT-END.

000310 READ-ASSETT-NEXT.
000320	     READ ASSETT NEXT WITH IGNORE LOCK.
000330     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000340	       GO TO READ-ASSETT-EXIT.
000210	     PERFORM READ-ERROR.
000350	   IF WS-ERROR = 3
000360	       MOVE 50		 TO WS-F-ERROR
000370	       GO TO READ-ASSETT-RESTORE.
	     GO TO READ-ASSETT-END.

000310 READ-ASSETT-NEXT-LOCK.
000320	     READ ASSETT NEXT WITH KEPT LOCK.
000330     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000340	       GO TO READ-ASSETT-EXIT.
000210	     PERFORM READ-ERROR.
000350	   IF WS-ERROR = 3
000360	       MOVE 50		 TO WS-F-ERROR
000370	       GO TO READ-ASSETT-RESTORE.
000410	   IF WS-ERROR = 1
	       MOVE W02-ASSETT	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000420	       PERFORM LOCKED-RECORD
000430	       GO TO READ-ASSETT-NEXT-LOCK.
000250	   IF WS-ERROR = 8
000260	       UNLOCK ASSETT
000270	       GO TO READ-ASSETT-NEXT-LOCK.
	     GO TO READ-ASSETT-END.

000310 READ-ASSETT-PREV.
000320	     READ ASSETT PREVIOUS WITH IGNORE LOCK.
000330     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000340	       GO TO READ-ASSETT-EXIT.
000210	     PERFORM READ-ERROR.
000350	   IF WS-ERROR = 3
000360	       MOVE 50		 TO WS-F-ERROR
000370	       GO TO READ-ASSETT-RESTORE.
	     GO TO READ-ASSETT-END.

000020 START-AT-ASST-CODE.
000030	     START ASSETT
	       KEY >= ASS-CODE.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-ASSETT-EXIT.
000210	     PERFORM READ-ERROR.
	   IF WS-ERROR = 3
	       MOVE 50		 TO WS-F-ERROR
000050	       GO TO READ-ASSETT-RESTORE.
	     GO TO READ-ASSETT-END.

000020 START-AT-ASST-DESC.
000030	     START ASSETT
	       KEY >= ASS-DKEY.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-ASSETT-EXIT.
000210	     PERFORM READ-ERROR.
	   IF WS-ERROR = 3
	       MOVE 50		 TO WS-F-ERROR
000050	       GO TO READ-ASSETT-RESTORE.

000470 READ-ASSETT-END.
	   IF NOT (WS-ERROR = ZERO)
	       STOP RUN.

       READ-ASSETT-RESTORE.
	     PERFORM RESTORE-SCREEN.
	     MOVE "N"		 TO WS-SKIP.

000470 READ-ASSETT-EXIT.
000480       EXIT.
