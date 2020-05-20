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
006260 READ-GLINST.
006270	     READ GLINST	 WITH IGNORE LOCK.
006280     IF WS-STATUS = "00"
006290         GO TO READ-GLINST-EXIT.
006300     IF WS-STAT1 = "2" OR "3" OR "4"
006310         MOVE 10 TO WS-F-ERROR
006320         PERFORM READ-ERROR.
006360       MOVE 10 TO WS-F-ERROR.
006370       PERFORM READ-ERROR.
006380       GO TO READ-GLINST-EXIT.

006390 READ-GLINST-CHECK.
006400	     READ GLINST	 WITH IGNORE LOCK.
006410     IF WS-STATUS = "00"
006420         GO TO READ-GLINST-EXIT.
006430     IF WS-STATUS = "23"
006440         MOVE 1 TO WS-INDG
006450         GO TO READ-GLINST-EXIT.
006460     IF WS-STAT1 = "2" OR "3" OR "4"
006470         MOVE 10 TO WS-F-ERROR
006480         PERFORM READ-ERROR.
006520       MOVE 10 TO WS-F-ERROR.
006530       PERFORM READ-ERROR.

006540 READ-GLINST-EXIT.
006550       EXIT.
