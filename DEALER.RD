      *
      *    ****  *****	***  *	   ***** ****	   ***** *** *	   *****
      *    *   * *     *   * *	   *	 *   *	   *	  *  *	   *
      *    *   * *     *   * *	   *	 *   *	   *	  *  *	   *
      *    *   * ***   ***** *	   ***	 ****	   ***	  *  *	   ***
      *    *   * *     *   * *	   *	 *  *	   *	  *  *	   *
      *    *   * *     *   * *	   *	 *   *	   *	  *  *	   *
      *    ****  ***** *   * ***** ***** *   *	   *	 *** ***** *****
      *
000020 READ-DEALER.
000030	     READ DEALER WITH IGNORE LOCK
	       KEY IS DEL-KEY.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-DEALER-EXIT.
           IF WS-STATUS = "23"
	       MOVE 44		 TO WS-F-ERROR
000050	       GO TO READ-DEALER-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 44		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
	     GO TO READ-DEALER-EXIT.

000150 READ-DEALER-DEPT.
000160	     READ DEALER WITH
	       KEY IS DEL-DKEY.
000170     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000180	       GO TO READ-DEALER-EXIT.
           IF WS-STATUS = "23"
	       MOVE 44		 TO WS-F-ERROR
000050	       GO TO READ-DEALER-EXIT.
000190     IF WS-STAT1 = "2" OR "3" OR "4"
000200	       MOVE 44		 TO WS-F-ERROR
000210         PERFORM READ-ERROR.
000220     IF RECORD-LOCKED
	       MOVE W02-DEALER	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000230         PERFORM LOCKED-RECORD
000240	       GO TO READ-DEALER-LOCK.
000250     IF TOO-MANY-LOCKS
000260	       UNLOCK DEALER
000270	       GO TO READ-DEALER-LOCK.
	     GO TO READ-DEALER-EXIT.

000310 READ-DEALER-NEXT.
000320	     READ DEALER NEXT WITH IGNORE LOCK.
000330     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000340	       GO TO READ-DEALER-EXIT.
000350     IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
000360	       MOVE 44		 TO WS-F-ERROR
000370	       GO TO READ-DEALER-EXIT.
000380     IF WS-STAT1 = "2" OR "3" OR "4"
000390	       MOVE 44		 TO WS-F-ERROR
000400         PERFORM READ-ERROR.
	     GO TO READ-DEALER-EXIT.

000310 READ-DEALER-NEXT-LOCK.
000320	     READ DEALER NEXT WITH KEPT LOCK.
000330     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000340	       GO TO READ-DEALER-EXIT.
000350     IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
000360	       MOVE 44		 TO WS-F-ERROR
000370	       GO TO READ-DEALER-EXIT.
000380     IF WS-STAT1 = "2" OR "3" OR "4"
000390	       MOVE 44		 TO WS-F-ERROR
000400         PERFORM READ-ERROR.
000410     IF RECORD-LOCKED
	       MOVE W02-DEALER	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000420	       PERFORM LOCKED-RECORD
000430	       GO TO READ-DEALER-NEXT-LOCK.
000250     IF TOO-MANY-LOCKS
000260	       UNLOCK DEALER
000270	       GO TO READ-DEALER-NEXT-LOCK.
	     GO TO READ-DEALER-EXIT.

000310 READ-DEALER-PREV.
000320	     READ DEALER PREVIOUS WITH IGNORE LOCK.
000330     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000340	       GO TO READ-DEALER-EXIT.
000350     IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
000360	       MOVE 44		 TO WS-F-ERROR
000370	       GO TO READ-DEALER-EXIT.
000380     IF WS-STAT1 = "2" OR "3" OR "4"
000390	       MOVE 44		 TO WS-F-ERROR
000400         PERFORM READ-ERROR.
	     GO TO READ-DEALER-EXIT.

000020 START-AT-DEALER-ACT.
000030	     START DEALER
	       KEY >= DEL-KEY.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-DEALER-EXIT.
           IF WS-STATUS = "23"
	       MOVE 44		 TO WS-F-ERROR
000050	       GO TO READ-DEALER-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 44		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
	     GO TO READ-DEALER-EXIT.

000470 READ-DEALER-EXIT.
000480       EXIT.
