      *
      *    ****  *****	***  *****  ***  *****	   ***** *** *	   *****
      *    *   * *     *   *   *   *   *   *	   *	  *  *	   *
      *    *   * *     *   *   *   *   *   *	   *	  *  *	   *
      *    *   * ***   *****   *   *   *   *	   ***	  *  *	   ***
      *    *   * *     *   *   *   *   *   *	   *	  *  *	   *
      *    *   * *     *   *   *   *   *   *	   *	  *  *	   *
      *    ****  ***** *   *   *    ***    *	   *	 *** ***** *****
      *
000020 READ-DEATOT.
000030	     READ DEATOT WITH IGNORE LOCK
	       KEY IS DET-KEY.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-DEATOT-EXIT.
           IF WS-STATUS = "23"
	       MOVE 45		 TO WS-F-ERROR
000050	       GO TO READ-DEATOT-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 45		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
	     GO TO READ-DEATOT-EXIT.

000150 READ-DEATOT-VALUE.
000160	     READ DEATOT WITH
	       KEY IS DET-VKEY.
000170     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000180	       GO TO READ-DEATOT-EXIT.
           IF WS-STATUS = "23"
	       MOVE 45		 TO WS-F-ERROR
000050	       GO TO READ-DEATOT-EXIT.
000190     IF WS-STAT1 = "2" OR "3" OR "4"
000200	       MOVE 45		 TO WS-F-ERROR
000210         PERFORM READ-ERROR.
000220     IF RECORD-LOCKED
	       MOVE W02-DEATOT	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000230         PERFORM LOCKED-RECORD
000240	       GO TO READ-DEATOT-LOCK.
000250     IF TOO-MANY-LOCKS
000260	       UNLOCK DEATOT
000270	       GO TO READ-DEATOT-LOCK.
	     GO TO READ-DEATOT-EXIT.

000310 READ-DEATOT-NEXT.
000320	     READ DEATOT NEXT WITH IGNORE LOCK.
000330     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000340	       GO TO READ-DEATOT-EXIT.
000350     IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
000360	       MOVE 45		 TO WS-F-ERROR
000370	       GO TO READ-DEATOT-EXIT.
000380     IF WS-STAT1 = "2" OR "3" OR "4"
000390	       MOVE 45		 TO WS-F-ERROR
000400         PERFORM READ-ERROR.
	     GO TO READ-DEATOT-EXIT.

000310 READ-DEATOT-NEXT-LOCK.
000320	     READ DEATOT NEXT WITH KEPT LOCK.
000330     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000340	       GO TO READ-DEATOT-EXIT.
000350     IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
000360	       MOVE 45		 TO WS-F-ERROR
000370	       GO TO READ-DEATOT-EXIT.
000380     IF WS-STAT1 = "2" OR "3" OR "4"
000390	       MOVE 45		 TO WS-F-ERROR
000400         PERFORM READ-ERROR.
000410     IF RECORD-LOCKED
	       MOVE W02-DEATOT	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000420	       PERFORM LOCKED-RECORD
000430	       GO TO READ-DEATOT-NEXT-LOCK.
000250     IF TOO-MANY-LOCKS
000260	       UNLOCK DEATOT
000270	       GO TO READ-DEATOT-NEXT-LOCK.
	     GO TO READ-DEATOT-EXIT.

000310 READ-DEATOT-PREV.
000320	     READ DEATOT PREVIOUS WITH IGNORE LOCK.
000330     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000340	       GO TO READ-DEATOT-EXIT.
000350     IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
000360	       MOVE 45		 TO WS-F-ERROR
000370	       GO TO READ-DEATOT-EXIT.
000380     IF WS-STAT1 = "2" OR "3" OR "4"
000390	       MOVE 45		 TO WS-F-ERROR
000400         PERFORM READ-ERROR.
	     GO TO READ-DEATOT-EXIT.

000020 START-AT-DPT-ACT.
000030	     START DEATOT
	       KEY >= DET-KEY.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-DEATOT-EXIT.
           IF WS-STATUS = "23"
	       MOVE 45		 TO WS-F-ERROR
000050	       GO TO READ-DEATOT-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 45		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
	     GO TO READ-DEATOT-EXIT.

000020 START-AT-DPT-VAL.
000030	     START DEATOT
	       KEY >= DET-VKEY.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-DEATOT-EXIT.
           IF WS-STATUS = "23"
	       MOVE 45		 TO WS-F-ERROR
000050	       GO TO READ-DEATOT-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 45		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
	     GO TO READ-DEATOT-EXIT.

000470 READ-DEATOT-EXIT.
000480       EXIT.
