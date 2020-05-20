      *
      *     ***  *     ****  *	 * ****  ****	  ***** *** *	  *****
      *    *   * *     *   * *	 * *   * *   *	  *	 *  *	  *
      *    *	 *     *   * *	 * *   * *   *	  *	 *  *	  *
      *    *	 *     ****  ***** *   * ****	  ***	 *  *	  ***
      *    *  ** *     *   * *	 * *   * *   *	  *	 *  *	  *
      *    *   * *     *   * *	 * *   * *   *	  *	 *  *	  *
      *     ***  ***** ****  *	 * ****  *   *	  *	*** ***** *****
      *
000020 READ-GLBHDR.
000030	     READ GLBHDR WITH IGNORE LOCK
	       KEY IS GBH-KEY.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-GLBHDR-EXIT.
000040     IF WS-STATUS = "23"
	       MOVE 51		 TO WS-F-ERROR
000050	       GO TO READ-GLBHDR-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 51		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
	     GO TO READ-GLBHDR-EXIT.

000020 READ-GLBHDR-LOCK.
000030	     READ GLBHDR WITH KEPT LOCK
	       KEY IS GBH-KEY.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-GLBHDR-EXIT.
000040     IF WS-STATUS = "23"
	       MOVE 51		 TO WS-F-ERROR
000050	       GO TO READ-GLBHDR-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 51		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
000090     IF RECORD-LOCKED
000070	       MOVE 51		 TO WS-F-ERROR.
	     GO TO READ-GLBHDR-EXIT.

000020 READ-GLBHDR-NEXT.
000030	     READ GLBHDR NEXT.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-GLBHDR-EXIT.
000040     IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
	       MOVE 51		 TO WS-F-ERROR
000050	       GO TO READ-GLBHDR-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 51		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
000090     IF RECORD-LOCKED
	       MOVE W02-GLBHDR	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000100         PERFORM LOCKED-RECORD
000110	       GO TO READ-GLBHDR-NEXT.
	     GO TO READ-GLBHDR-EXIT.

000020 READ-GLBHDR-PREV.
000030	     READ GLBHDR PREVIOUS.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-GLBHDR-EXIT.
000040     IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
	       MOVE 51		 TO WS-F-ERROR
000050	       GO TO READ-GLBHDR-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 51		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
000090     IF RECORD-LOCKED
	       MOVE W02-GLBHDR	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000100         PERFORM LOCKED-RECORD
000110	       GO TO READ-GLBHDR-PREV.
	     GO TO READ-GLBHDR-EXIT.

       START-AT-GLBHDR-KEY.
000030	     START GLBHDR
	       KEY >= GBH-KEY.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-GLBHDR-EXIT.
000040     IF WS-STATUS = "23"
	       MOVE 51		 TO WS-F-ERROR
000050	       GO TO READ-GLBHDR-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 51		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
	     GO TO READ-GLBHDR-EXIT.

000150 READ-GLBHDR-EXIT.
000160       EXIT.
