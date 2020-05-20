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
009060 READ-MEMDEB.
009070	     READ MEMDEB	 WITH IGNORE LOCK
	       KEY IS MEM-KEY.
009080     IF WS-STATUS = "00"
006890         MOVE ZERO         TO WS-F-ERROR
009090	       GO TO READ-MEMDEB-EXIT.
009900	   IF WS-STATUS = "23"
006890	       MOVE 62		 TO WS-F-ERROR
006870         GO TO READ-DEBMEM-EXIT.
009100	   IF WS-STAT1 = "2" OR "3" OR "4"
009110	       MOVE 62		 TO WS-F-ERROR
009120         PERFORM READ-ERROR.
009160	     MOVE 62		 TO WS-F-ERROR.
	     GO TO READ-MEMDEB-EXIT.

006840 READ-MEMDEB-NEXT.
006850	     READ MEMDEB NEXT	 WITH IGNORE LOCK.
006860     IF WS-STATUS = "00"
006890         MOVE ZERO         TO WS-F-ERROR
006870	       GO TO READ-MEMDEB-EXIT.
009900     IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
006890	       MOVE 62		 TO WS-F-ERROR
006870	       GO TO READ-MEMDEB-EXIT.
006880     IF WS-STAT1 = "2" OR "3" OR "4"
006890	       MOVE 62		 TO WS-F-ERROR
006900         PERFORM READ-ERROR.
	     GO TO READ-MEMDEB-EXIT.

006840 START-AT-DEBTAC.
006850	     START MEMDEB
	       KEY >= MEM-KEY.
006860     IF WS-STATUS = "00"
006890         MOVE ZERO         TO WS-F-ERROR
006870	       GO TO READ-MEMDEB-EXIT.
009900     IF WS-STATUS = "23"
006890	       MOVE 62		 TO WS-F-ERROR
006870	       GO TO READ-MEMDEB-EXIT.
006880     IF WS-STAT1 = "2" OR "3" OR "4"
006890	       MOVE 62		 TO WS-F-ERROR
006900         PERFORM READ-ERROR.

009180 READ-MEMDEB-EXIT.
009190       EXIT.
