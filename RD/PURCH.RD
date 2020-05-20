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
009420 READ-PURCH.
009430       READ PURCH.
009440     IF WS-STATUS = "00"
009450         GO TO READ-PURCH-EXIT.
009460     IF WS-STAT1 = "2" OR "3" OR "4"
009470         MOVE 17           TO WS-F-ERROR
009480         PERFORM READ-ERROR.
009490     IF RECORD-LOCKED
009500         PERFORM LOCKED-RECORD
009510         GO TO READ-PURCH.
009520       MOVE 17             TO WS-F-ERROR.
009530       PERFORM READ-ERROR.
      
009540 READ-PURCH-EXIT.
009550       EXIT.
