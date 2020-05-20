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
008650 READ-GLJRN.
008660       READ GLJRN.
008670     IF WS-STATUS = "00"
008680         GO TO READ-GLJRN-EXIT.
008690     IF WS-STAT1 = "2" OR "3" OR "4"
008700         MOVE 26           TO WS-F-ERROR
008710         PERFORM READ-ERROR.
008720     IF RECORD-LOCKED
008730         PERFORM LOCKED-RECORD
008740         GO TO READ-GLJRN.
008750       MOVE 26             TO WS-F-ERROR.
008760       PERFORM READ-ERROR.

008770 READ-GLJRN-EXIT.
008780       EXIT.
