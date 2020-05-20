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
009060 READ-SALE.
009070       READ SALE.
009080     IF WS-STATUS = "00"
009090         GO TO READ-SALE-EXIT.
009100     IF WS-STAT1 = "2" OR "3" OR "4"
009110         MOVE 19           TO WS-F-ERROR
009120         PERFORM READ-ERROR.
009130     IF RECORD-LOCKED
009140         PERFORM LOCKED-RECORD
009150         GO TO READ-SALE.
009160       MOVE 19             TO WS-F-ERROR.
009170       PERFORM READ-ERROR.

009180 READ-SALE-EXIT.
009190       EXIT.
