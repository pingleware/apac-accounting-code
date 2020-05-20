      *
      *    ****  ***** ****  ****  ***** ****	   ***** *** *	   *****
      *    *   * *     *   * *	 * *	 *   *	   *	  *  *	   *
      *    *   * *     *   * *	 * *	 *   *	   *	  *  *	   *
      *    *   * ***   ****  *	 * ***	 ****	   ***	  *  *	   ***
      *    *   * *     *   * *	 * *	 *	   *	  *  *	   *
      *    *   * *     *   * *	 * *	 *	   *	  *  *	   *
      *    ****  ***** ****  ****  ***** *	   *	 *** ***** *****
      *
000020 READ-DEBDEP.
000030	     READ DEBDEP WITH IGNORE LOCK
	       KEY IS DBD-ACNO.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-DEBDEP-EXIT.
           IF WS-STATUS = "23"
	       MOVE 33		 TO WS-F-ERROR
000050	       GO TO READ-DEBDEP-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 33		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
	     GO TO READ-DEBDEP-EXIT.

000150 READ-DEBDEP-LOCK.
000160	     READ DEBDEP WITH KEPT LOCK
	       KEY IS DBD-ACNO.
000170     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000180	       GO TO READ-DEBDEP-EXIT.
           IF WS-STATUS = "23"
	       MOVE 33		 TO WS-F-ERROR
000050	       GO TO READ-DEBDEP-EXIT.
000190     IF WS-STAT1 = "2" OR "3" OR "4"
000200	       MOVE 33		 TO WS-F-ERROR
000210         PERFORM READ-ERROR.
000220     IF RECORD-LOCKED
	       MOVE W02-DEBDEP	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000230         PERFORM LOCKED-RECORD
000240	       GO TO READ-DEBDEP-LOCK.
000250     IF TOO-MANY-LOCKS
000260	       UNLOCK DEBDEP
000270	       GO TO READ-DEBDEP-LOCK.
	     GO TO READ-DEBDEP-EXIT.

000310 READ-DEBDEP-NEXT.
000320	     READ DEBDEP NEXT WITH IGNORE LOCK.
000330     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000340	       GO TO READ-DEBDEP-EXIT.
000350     IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
000360	       MOVE 33		 TO WS-F-ERROR
000370	       GO TO READ-DEBDEP-EXIT.
000380     IF WS-STAT1 = "2" OR "3" OR "4"
000390	       MOVE 33		 TO WS-F-ERROR
000400         PERFORM READ-ERROR.
	     GO TO READ-DEBDEP-EXIT.

000310 READ-DEBDEP-NEXT-LOCK.
000320	     READ DEBDEP NEXT WITH KEPT LOCK.
000330     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000340	       GO TO READ-DEBDEP-EXIT.
000350     IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
000360	       MOVE 33		 TO WS-F-ERROR
000370	       GO TO READ-DEBDEP-EXIT.
000380     IF WS-STAT1 = "2" OR "3" OR "4"
000390	       MOVE 33		 TO WS-F-ERROR
000400         PERFORM READ-ERROR.
000410     IF RECORD-LOCKED
	       MOVE W02-DEBDEP	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000420	       PERFORM LOCKED-RECORD
000430	       GO TO READ-DEBDEP-NEXT-LOCK.
000250     IF TOO-MANY-LOCKS
000260	       UNLOCK DEBDEP
000270	       GO TO READ-DEBDEP-NEXT-LOCK.
	     GO TO READ-DEBDEP-EXIT.

000310 READ-DEBDEP-PREV.
000320	     READ DEBDEP PREVIOUS WITH IGNORE LOCK.
000330     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000340	       GO TO READ-DEBDEP-EXIT.
000350     IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
000360	       MOVE 33		 TO WS-F-ERROR
000370	       GO TO READ-DEBDEP-EXIT.
000380     IF WS-STAT1 = "2" OR "3" OR "4"
000390	       MOVE 33		 TO WS-F-ERROR
000400         PERFORM READ-ERROR.
	     GO TO READ-DEBDEP-EXIT.

000020 START-AT-DDPT-AC.
000030	     START DEBDEP
	       KEY >= DBD-ACNO.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-DEBDEP-EXIT.
           IF WS-STATUS = "23"
	       MOVE 33		 TO WS-F-ERROR
000050	       GO TO READ-DEBDEP-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 33		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
	     GO TO READ-DEBDEP-EXIT.

000470 READ-DEBDEP-EXIT.
000480       EXIT.
