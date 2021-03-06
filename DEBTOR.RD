      *
      *    ****  ***** ****  *****  ***  ****     ***** *** *     *****
      *    *   * *     *   *   *   *   * *   *    *      *  *     *
      *    *   * *     *   *   *   *   * *   *    *      *  *     *
      *    *   * ***   ****    *   *   * ****     ***    *  *     ***
      *    *   * *     *   *   *   *   * *   *    *      *  *     *
      *    *   * *     *   *   *   *   * *   *    *      *  *     *
      *    ****  ***** ****    *    ***  *   *    *     *** ***** *****
      *
000020 READ-DEBTOR.
000030	     READ DEBTOR WITH IGNORE LOCK
               KEY IS DEB-ACNO.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050         GO TO READ-DEBTOR-EXIT.
           IF WS-STATUS = "23"
               MOVE 6            TO WS-F-ERROR
000050         GO TO READ-DEBTOR-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070         MOVE 6            TO WS-F-ERROR
000080         PERFORM READ-ERROR.
             GO TO READ-DEBTOR-EXIT.

000150 READ-DEBTOR-LOCK.
000160       READ DEBTOR WITH KEPT LOCK
               KEY IS DEB-ACNO.
000170     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000180         GO TO READ-DEBTOR-EXIT.
           IF WS-STATUS = "23"
               MOVE 6            TO WS-F-ERROR
000050         GO TO READ-DEBTOR-EXIT.
000190     IF WS-STAT1 = "2" OR "3" OR "4"
000200         MOVE 6            TO WS-F-ERROR
000210         PERFORM READ-ERROR.
000220     IF RECORD-LOCKED
	       MOVE W02-DEBTOR	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000230         PERFORM LOCKED-RECORD
000240         GO TO READ-DEBTOR-LOCK.
000250     IF TOO-MANY-LOCKS
000260         UNLOCK DEBTOR
000270         GO TO READ-DEBTOR-LOCK.
             GO TO READ-DEBTOR-EXIT.

000310 READ-DEBTOR-NEXT.
000320	     READ DEBTOR NEXT WITH IGNORE LOCK.
000330     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000340         GO TO READ-DEBTOR-EXIT.
000350     IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
000360         MOVE 6            TO WS-F-ERROR
000370         GO TO READ-DEBTOR-EXIT.
000380     IF WS-STAT1 = "2" OR "3" OR "4"
000390         MOVE 6            TO WS-F-ERROR
000400         PERFORM READ-ERROR.
             GO TO READ-DEBTOR-EXIT.

000310 READ-DEBTOR-NEXT-LOCK.
000320       READ DEBTOR NEXT WITH KEPT LOCK.
000330     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000340         GO TO READ-DEBTOR-EXIT.
000350     IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
000360         MOVE 6            TO WS-F-ERROR
000370         GO TO READ-DEBTOR-EXIT.
000380     IF WS-STAT1 = "2" OR "3" OR "4"
000390         MOVE 6            TO WS-F-ERROR
000400         PERFORM READ-ERROR.
000410     IF RECORD-LOCKED
	       MOVE W02-DEBTOR	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000420	       PERFORM LOCKED-RECORD
000430         GO TO READ-DEBTOR-NEXT-LOCK.
000250     IF TOO-MANY-LOCKS
000260         UNLOCK DEBTOR
000270         GO TO READ-DEBTOR-NEXT-LOCK.
             GO TO READ-DEBTOR-EXIT.

000310 READ-DEBTOR-PREV.
000320	     READ DEBTOR PREVIOUS WITH IGNORE LOCK.
000330     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000340         GO TO READ-DEBTOR-EXIT.
000350     IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
000360         MOVE 6            TO WS-F-ERROR
000370         GO TO READ-DEBTOR-EXIT.
000380     IF WS-STAT1 = "2" OR "3" OR "4"
000390         MOVE 6            TO WS-F-ERROR
000400         PERFORM READ-ERROR.
             GO TO READ-DEBTOR-EXIT.

000020 START-AT-DEBT-AC.
000030       START DEBTOR
               KEY >= DEB-ACNO.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050         GO TO READ-DEBTOR-EXIT.
           IF WS-STATUS = "23"
               MOVE 6            TO WS-F-ERROR
000050         GO TO READ-DEBTOR-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070         MOVE 6            TO WS-F-ERROR
000080         PERFORM READ-ERROR.
             GO TO READ-DEBTOR-EXIT.

000020 START-AT-DEBT-NAME.
000030       START DEBTOR
               KEY >= DEB-NKEY.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050         GO TO READ-DEBTOR-EXIT.
           IF WS-STATUS = "23"
               MOVE 6            TO WS-F-ERROR
000050         GO TO READ-DEBTOR-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070         MOVE 6            TO WS-F-ERROR
000080         PERFORM READ-ERROR.
             GO TO READ-DEBTOR-EXIT.

000470 READ-DEBTOR-EXIT.
000480       EXIT.
