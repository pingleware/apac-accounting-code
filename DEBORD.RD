      *
      *    ****  ***** ****   ***  ****	 ****	  ***** *** *	  *****
      *    *   * *     *   * *	 * *   * *   *	  *	 *  *	  *
      *    *   * *     *   * *	 * *   * *   *	  *	 *  *	  *
      *    *   * ***   ****  *	 * ****	 *   *	  ***	 *  *	  ***
      *    *   * *     *   * *	 * *   * *   *	  *	 *  *	  *
      *    *   * *     *   * *	 * *   * *   *	  *	 *  *	  *
      *    ****  ***** ****   ***  *   * ****	  *	*** ***** *****
      *
000020 READ-DEBORD.
000030	     READ DEBORD
	       KEY IS DOR-KEY.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-DEBORD-EXIT.
           IF WS-STATUS = "23"
	       MOVE 53		 TO WS-F-ERROR
000050	       GO TO READ-DEBORD-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 53		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
000090     IF RECORD-LOCKED
	       MOVE W02-DEBORD	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000100         PERFORM LOCKED-RECORD
000110	       GO TO READ-DEBORD.
000050	     GO TO READ-DEBORD-EXIT.

000020 READ-DEBORD-NEXT.
000030	     READ DEBORD NEXT.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-DEBORD-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
	       MOVE 53		 TO WS-F-ERROR
000050	       GO TO READ-DEBORD-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 53		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
000090     IF RECORD-LOCKED
	       MOVE W02-DEBORD	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000100         PERFORM LOCKED-RECORD
000110	       GO TO READ-DEBORD-NEXT.
000050	     GO TO READ-DEBORD-EXIT.

000020 READ-DEBORD-PREV.
000030	     READ DEBORD PREVIOUS.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-DEBORD-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
	       MOVE 53		 TO WS-F-ERROR
000050	       GO TO READ-DEBORD-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 53		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
000090     IF RECORD-LOCKED
	       MOVE W02-DEBORD	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000100         PERFORM LOCKED-RECORD
000110	       GO TO READ-DEBORD-PREV.
000050	     GO TO READ-DEBORD-EXIT.

000020 START-AT-DORD-KEY.
000030	     START DEBORD
	       KEY >= DOR-KEY.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-DEBORD-EXIT.
           IF WS-STATUS = "23"
	       MOVE 53		 TO WS-F-ERROR
000050	       GO TO READ-DEBORD-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 53		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
000050	     GO TO READ-DEBORD-EXIT.

000020 START-AT-DORD-DKEY.
000030	     START DEBORD
	       KEY >= DOR-DKEY.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-DEBORD-EXIT.
           IF WS-STATUS = "23"
	       MOVE 53		 TO WS-F-ERROR
000050	       GO TO READ-DEBORD-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 53		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.

000050	     GO TO READ-DEBORD-EXIT.

000020 START-AT-DORD-RKEY.
000030	     START DEBORD
	       KEY >= DOR-RKEY.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-DEBORD-EXIT.
           IF WS-STATUS = "23"
	       MOVE 53		 TO WS-F-ERROR
000050	       GO TO READ-DEBORD-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 53		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.

000150 READ-DEBORD-EXIT.
000160       EXIT.
