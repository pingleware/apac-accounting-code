      *
      *    ****  ****  *****  ***   ***  *   *	  ***** *** *	  *****
      *    *   * *   *	 *   *	 * *   * *   *	  *	 *  *	  *
      *    *   * *   *	 *   *	   *   * **  *	  *	 *  *	  *
      *    *   * ****	 *   *	   *   * * * *	  ***	 *  *	  ***
      *    *   * *   *	 *   *	   *   * *  **	  *	 *  *	  *
      *    *   * *   *   *   *   * *   * *   *    *      *  *     *
      *    ****  ****	 *    ***   ***  *   *	  *	*** ***** *****
      *
000020 READ-DBTCON.
000030	     READ DBTCON
	       KEY IS DBT-KEY.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-DBTCON-EXIT.
           IF WS-STATUS = "23"
	       MOVE 64		 TO WS-F-ERROR
000050	       GO TO READ-DBTCON-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 64		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
000090     IF RECORD-LOCKED
	       MOVE W02-DBTCON	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000100         PERFORM LOCKED-RECORD
000110	       GO TO READ-DBTCON.
000050	     GO TO READ-DBTCON-EXIT.

000020 READ-DBTCON-NEXT.
000030	     READ DBTCON NEXT.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-DBTCON-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
	       MOVE 64		 TO WS-F-ERROR
000050	       GO TO READ-DBTCON-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 64		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
000090     IF RECORD-LOCKED
	       MOVE W02-DBTCON	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000100         PERFORM LOCKED-RECORD
000110	       GO TO READ-DBTCON-NEXT.
000050	     GO TO READ-DBTCON-EXIT.

000020 READ-DBTCON-PREV.
000030	     READ DBTCON PREVIOUS.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-DBTCON-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
	       MOVE 64		 TO WS-F-ERROR
000050	       GO TO READ-DBTCON-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 64		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
000090     IF RECORD-LOCKED
	       MOVE W02-DBTCON	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000100         PERFORM LOCKED-RECORD
000110	       GO TO READ-DBTCON-PREV.
000050	     GO TO READ-DBTCON-EXIT.

000020 START-AT-DBT-KEY.
000030	     START DBTCON
	       KEY >= DBT-KEY.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-DBTCON-EXIT.
           IF WS-STATUS = "23"
	       MOVE 64		 TO WS-F-ERROR
000050	       GO TO READ-DBTCON-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 64		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
	     GO TO READ-DBTCON-EXIT.

000150 READ-DBTCON-EXIT.
000160       EXIT.
