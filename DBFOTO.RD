      *
      *    ****  ****  *****  ***  *****  ***	  ***** *** *	  *****
      *    *   * *   * *     *	 *   *	 *   *	  *	 *  *	  *
      *    *   * *   * *     *	 *   *	 *   *	  *	 *  *	  *
      *    *   * ****  ***   *	 *   *	 *   *	  ***	 *  *	  ***
      *    *   * *   * *     *	 *   *	 *   *	  *	 *  *	  *
      *    *   * *   * *     *	 *   *	 *   *	  *	 *  *	  *
      *    ****  ****  *      ***    *	  ***	  *	*** ***** *****
      *
000020 READ-DBFOTO.
000030	     READ DBFOTO
	       KEY IS DBF-KEY.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-DBFOTO-EXIT.
           IF WS-STATUS = "23"
	       MOVE 42		 TO WS-F-ERROR
000050	       GO TO READ-DBFOTO-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 42		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
000090     IF RECORD-LOCKED
	       MOVE W02-DBFOTO	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000100         PERFORM LOCKED-RECORD
000110	       GO TO READ-DBFOTO.
000050	     GO TO READ-DBFOTO-EXIT.

000020 READ-DBFOTO-NEXT.
000030	     READ DBFOTO NEXT.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-DBFOTO-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
	       MOVE 42		 TO WS-F-ERROR
000050	       GO TO READ-DBFOTO-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 42		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
000090     IF RECORD-LOCKED
	       MOVE W02-DBFOTO	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000100         PERFORM LOCKED-RECORD
000110	       GO TO READ-DBFOTO-NEXT.
000050	     GO TO READ-DBFOTO-EXIT.

000020 READ-DBFOTO-PREV.
000030	     READ DBFOTO PREVIOUS.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-DBFOTO-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
	       MOVE 42		 TO WS-F-ERROR
000050	       GO TO READ-DBFOTO-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 42		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
000090     IF RECORD-LOCKED
	       MOVE W02-DBFOTO	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000100         PERFORM LOCKED-RECORD
000110	       GO TO READ-DBFOTO-PREV.
000050	     GO TO READ-DBFOTO-EXIT.

000020 START-AT-DBFT-KEY.
000030	     START DBFOTO
	       KEY >= DBF-KEY.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-DBFOTO-EXIT.
           IF WS-STATUS = "23"
	       MOVE 42		 TO WS-F-ERROR
000050	       GO TO READ-DBFOTO-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 42		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
	     GO TO READ-DBFOTO-EXIT.

000150 READ-DBFOTO-EXIT.
000160       EXIT.
