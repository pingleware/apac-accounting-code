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
000020 READ-SYSUSER.
000030	     READ SYSUSER WITH IGNORE LOCK
	       KEY IS USE-NO.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-SYSUSER-EXIT.
           IF WS-STATUS = "23"
	       MOVE 64		 TO WS-F-ERROR
000050	       GO TO READ-SYSUSER-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 64		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
	     GO TO READ-SYSUSER-EXIT.

000150 READ-SYSUSER-LOCK.
000160	     READ SYSUSER WITH KEPT LOCK
	       KEY IS USE-NO.
000170     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000180	       GO TO READ-SYSUSER-EXIT.
           IF WS-STATUS = "23"
	       MOVE 64		 TO WS-F-ERROR
000050	       GO TO READ-SYSUSER-EXIT.
000190     IF WS-STAT1 = "2" OR "3" OR "4"
000200	       MOVE 64		 TO WS-F-ERROR
000210         PERFORM READ-ERROR.
000220     IF RECORD-LOCKED
	       MOVE W02-SYSUSER	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000230         PERFORM LOCKED-RECORD
000240	       GO TO READ-SYSUSER-LOCK.
000250     IF TOO-MANY-LOCKS
000260	       UNLOCK SYSUSER
000270	       GO TO READ-SYSUSER-LOCK.
	     GO TO READ-SYSUSER-EXIT.

000310 READ-SYSUSER-NEXT.
000320	     READ SYSUSER NEXT WITH IGNORE LOCK.
000330     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000340	       GO TO READ-SYSUSER-EXIT.
000350     IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
000360	       MOVE 64		 TO WS-F-ERROR
000370	       GO TO READ-SYSUSER-EXIT.
000380     IF WS-STAT1 = "2" OR "3" OR "4"
000390	       MOVE 64		 TO WS-F-ERROR
000400         PERFORM READ-ERROR.
	     GO TO READ-SYSUSER-EXIT.

000310 READ-SYSUSER-NEXT-LOCK.
000320	     READ SYSUSER NEXT WITH KEPT LOCK.
000330     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000340	       GO TO READ-SYSUSER-EXIT.
000350     IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
000360	       MOVE 64		 TO WS-F-ERROR
000370	       GO TO READ-SYSUSER-EXIT.
000380     IF WS-STAT1 = "2" OR "3" OR "4"
000390	       MOVE 64		 TO WS-F-ERROR
000400         PERFORM READ-ERROR.
000410     IF RECORD-LOCKED
	       MOVE W02-SYSUSER	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000420	       PERFORM LOCKED-RECORD
000430	       GO TO READ-SYSUSER-NEXT-LOCK.
000250     IF TOO-MANY-LOCKS
000260	       UNLOCK SYSUSER
000270	       GO TO READ-SYSUSER-NEXT-LOCK.
	     GO TO READ-SYSUSER-EXIT.

000310 READ-SYSUSER-PREV.
000320	     READ SYSUSER PREVIOUS WITH IGNORE LOCK.
000330     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000340	       GO TO READ-SYSUSER-EXIT.
000350     IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
000360	       MOVE 64		 TO WS-F-ERROR
000370	       GO TO READ-SYSUSER-EXIT.
000380     IF WS-STAT1 = "2" OR "3" OR "4"
000390	       MOVE 64		 TO WS-F-ERROR
000400         PERFORM READ-ERROR.
	     GO TO READ-SYSUSER-EXIT.

000020 START-AT-USER-AC.
000030	     START SYSUSER
	       KEY >= USE-NO.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-SYSUSER-EXIT.
           IF WS-STATUS = "23"
	       MOVE 64		 TO WS-F-ERROR
000050	       GO TO READ-SYSUSER-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 64		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
	     GO TO READ-SYSUSER-EXIT.

000020 START-AT-USER-NAME.
000030	     START SYSUSER
	       KEY >= USE-NKEY.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-SYSUSER-EXIT.
           IF WS-STATUS = "23"
	       MOVE 64		 TO WS-F-ERROR
000050	       GO TO READ-SYSUSER-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 64		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
	     GO TO READ-SYSUSER-EXIT.

000470 READ-SYSUSER-EXIT.
000480       EXIT.
