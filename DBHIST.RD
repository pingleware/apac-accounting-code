      *
      *    ****  ****  *   * ***  ***  *****	 ***** *** *	 *****
      *    *   * *   * *   *  *  *   *	 *	 *	*  *	 *
      *    *   * *   * *   *  *  *	 *	 *	*  *	 *
      *    *   * ****  *****  *   ***	 *	 ***	*  *	 ***
      *    *   * *   * *   *  *      *	 *	 *	*  *	 *
      *    *   * *   * *   *  *  *   *	 *	 *	*  *	 *
      *    ****  ****  *   * ***  ***	 *	 *     *** ***** *****
      *
000020 READ-DBHIST.
000030	     READ DBHIST WITH IGNORE LOCK
	       KEY IS DBH-ACNO.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-DBHIST-EXIT.
           IF WS-STATUS = "23"
	       MOVE 56		 TO WS-F-ERROR
000050	       GO TO READ-DBHIST-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 56		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
	     GO TO READ-DBHIST-EXIT.

000310 READ-DBHIST-NEXT.
000320	     READ DBHIST NEXT WITH IGNORE LOCK.
000330     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000340	       GO TO READ-DBHIST-EXIT.
000350     IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
000360	       MOVE 56		 TO WS-F-ERROR
000370	       GO TO READ-DBHIST-EXIT.
000380     IF WS-STAT1 = "2" OR "3" OR "4"
000390	       MOVE 56		 TO WS-F-ERROR
000400         PERFORM READ-ERROR.
	     GO TO READ-DBHIST-EXIT.

000310 READ-DBHIST-PREV.
000320	     READ DBHIST PREVIOUS WITH IGNORE LOCK.
000330     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000340	       GO TO READ-DBHIST-EXIT.
000350     IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
000360	       MOVE 56		 TO WS-F-ERROR
000370	       GO TO READ-DBHIST-EXIT.
000380     IF WS-STAT1 = "2" OR "3" OR "4"
000390	       MOVE 56		 TO WS-F-ERROR
000400         PERFORM READ-ERROR.
	     GO TO READ-DBHIST-EXIT.

000020 START-AT-DEBH-AC.
000030	     START DBHIST
	       KEY >= DBH-ACNO.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-DBHIST-EXIT.
           IF WS-STATUS = "23"
	       MOVE 56		 TO WS-F-ERROR
000050	       GO TO READ-DBHIST-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 56		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
	     GO TO READ-DBHIST-EXIT.

000020 START-AT-DEBH-NAME.
000030	     START DBHIST
	       KEY >= DBH-NKEY.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-DBHIST-EXIT.
           IF WS-STATUS = "23"
	       MOVE 56		 TO WS-F-ERROR
000050	       GO TO READ-DBHIST-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 56		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
	     GO TO READ-DBHIST-EXIT.

000470 READ-DBHIST-EXIT.
000480       EXIT.
