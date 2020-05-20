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
000020 READ-CONTROL.
000030	     READ NETWORK IGNORE LOCK.
000040     IF WS-STATUS = "00"
               MOVE 1            TO WS-COUNT
000050         GO TO READ-CONTROL-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070         MOVE 2            TO WS-F-ERROR
000080         PERFORM READ-ERROR.
000120       MOVE 2              TO WS-F-ERROR.
000130       PERFORM READ-ERROR.
000140       GO TO READ-CONTROL-EXIT.

000160 READ-CONTROL-LOCK.
000170       READ NETWORK WITH KEPT LOCK.
000180     IF WS-STATUS = "00"
000190         GO TO READ-CONTROL-EXIT.
000200     IF WS-STAT1 = "2" OR "3" OR "4"
000210         MOVE 2            TO WS-F-ERROR
000220         PERFORM READ-ERROR.
000230     IF RECORD-LOCKED
               IF WS-NETKEY = 5
		   COMMIT
                   PERFORM LOCK-USERS-REC THRU LOCK-REC-EXIT
                           WS-COUNT TIMES
                   ADD 1         TO WS-COUNT
                   IF WS-COUNT > 25
                       MOVE 1    TO WS-COUNT
                   END-IF
000250             GO TO READ-CONTROL-LOCK
               ELSE
		   MOVE W02-NETWORK
				 TO WS-FILE
		   MOVE WS-NETKEY
				 TO WS-KEY
000240             PERFORM LOCKED-RECORD WS-COUNT TIMES
                   ADD 1         TO WS-COUNT
                   IF WS-COUNT > 20
                       MOVE 1    TO WS-COUNT
                   END-IF
000250             GO TO READ-CONTROL-LOCK.
000260     IF TOO-MANY-LOCKS
000270         UNLOCK NETWORK
000280         GO TO READ-CONTROL-LOCK.
000290       MOVE 2              TO WS-F-ERROR.
000300       PERFORM READ-ERROR.

000320 READ-CONTROL-EXIT.
000330       EXIT.
