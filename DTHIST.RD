      *
      *    ****  ***** *   * ***  ***  *****	 ***** *** *	 *****
      *    *   *   *   *   *  *  *   *	 *	 *	*  *	 *
      *    *   *   *   *   *  *  *	 *	 *	*  *	 *
      *    *   *   *   *****  *   ***	 *	 ***	*  *	 ***
      *    *   *   *   *   *  *      *	 *	 *	*  *	 *
      *    *   *   *   *   *  *  *   *	 *	 *	*  *	 *
      *    ****    *   *   * ***  ***	 *	 *     *** ***** *****
      *
000020 READ-DTHIST.
000030	     READ DTHIST
	       KEY IS TRH-KEY.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-DTHIST-EXIT.
           IF WS-STATUS = "23"
	       MOVE 55		 TO WS-F-ERROR
000050	       GO TO READ-DTHIST-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 55		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
000090     IF RECORD-LOCKED
	       MOVE W02-DTHIST	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000100         PERFORM LOCKED-RECORD
000110	       GO TO READ-DTHIST.
000050	     GO TO READ-DTHIST-EXIT.

000020 READ-DTHIST-NEXT.
000030	     READ DTHIST NEXT.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-DTHIST-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
	       MOVE 55		 TO WS-F-ERROR
000050	       GO TO READ-DTHIST-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 55		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
000090     IF RECORD-LOCKED
	       MOVE W02-DTHIST	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000100         PERFORM LOCKED-RECORD
000110	       GO TO READ-DTHIST-NEXT.
000050	     GO TO READ-DTHIST-EXIT.

000020 READ-DTHIST-PREV.
000030	     READ DTHIST PREVIOUS.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-DTHIST-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
	       MOVE 55		 TO WS-F-ERROR
000050	       GO TO READ-DTHIST-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 55		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
000090     IF RECORD-LOCKED
	       MOVE W02-DTHIST	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000100         PERFORM LOCKED-RECORD
000110	       GO TO READ-DTHIST-PREV.
000050	     GO TO READ-DTHIST-EXIT.

000020 START-AT-DTRH-KEY.
000030	     START DTHIST
	       KEY >= TRH-KEY.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-DTHIST-EXIT.
           IF WS-STATUS = "23"
	       MOVE 55		 TO WS-F-ERROR
000050	       GO TO READ-DTHIST-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 55		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
	     GO TO READ-DTHIST-EXIT.

000020 START-AT-DTRH-CODE.
000030	     START DTHIST
	       KEY >= TRH-TRSEQ.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-DTHIST-EXIT.
           IF WS-STATUS = "23"
	       MOVE 55		 TO WS-F-ERROR
000050	       GO TO READ-DTHIST-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 55		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
	     GO TO READ-DTHIST-EXIT.

000150 READ-DTHIST-EXIT.
000160       EXIT.
