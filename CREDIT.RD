      *
      *     ***  ****  ***** ****  *** *****     ***** *** *     *****
      *    *   * *   * *     *   *  *    *       *      *  *     *
      *    *     *   * *     *   *  *    *       *      *  *     *
      *    *     ****  ***   *   *  *    *       ***    *  *     ***
      *    *     *   * *     *   *  *    *       *      *  *     *
      *    *   * *   * *     *   *  *    *       *      *  *     *
      *     ***  *   * ***** ****  ***   *       *     *** ***** *****
      *
006080 READ-CREDIT.
006090	     READ CREDIT WITH IGNORE LOCK
               KEY IS P-NUMBER.
006100     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110         GO TO READ-CREDIT-EXIT.
           IF WS-STATUS = "23"
               MOVE 3            TO WS-F-ERROR
               GO TO READ-CREDIT-EXIT.
006120     IF WS-STAT1 = "2" OR "3" OR "4"
006130         MOVE 3            TO WS-F-ERROR
006140         PERFORM READ-ERROR.
006200       GO TO READ-CREDIT-EXIT.

006210 READ-CREDIT-LOCK.
006220       READ CREDIT WITH KEPT LOCK
               KEY IS P-NUMBER.
006230	   IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
006240         GO TO READ-CREDIT-EXIT.
           IF WS-STATUS = "23"
               MOVE 3            TO WS-F-ERROR
               GO TO READ-CREDIT-EXIT.
006250     IF WS-STAT1 = "2" OR "3" OR "4"
006260         MOVE 3            TO WS-F-ERROR
006270         PERFORM READ-ERROR.
006280     IF RECORD-LOCKED
	       MOVE W02-CREDIT	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
006290         PERFORM LOCKED-RECORD
006300         GO TO READ-CREDIT-LOCK.
006310     IF TOO-MANY-LOCKS
006320         UNLOCK CREDIT
006330         GO TO READ-CREDIT-LOCK.
006360       GO TO READ-CREDIT-EXIT.

006080 READ-CREDIT-NEXT.
006090	     READ CREDIT NEXT IGNORE LOCK.
006100	   IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
006110         GO TO READ-CREDIT-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
               MOVE 3            TO WS-F-ERROR
               GO TO READ-CREDIT-EXIT.
006120     IF WS-STAT1 = "2" OR "3" OR "4"
006130         MOVE 3            TO WS-F-ERROR
006140         PERFORM READ-ERROR.
006200       GO TO READ-CREDIT-EXIT.

006080 READ-CREDIT-NEXT-LOCK.
006090       READ CREDIT NEXT WITH KEPT LOCK.
006100	   IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
006110         GO TO READ-CREDIT-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
               MOVE 3            TO WS-F-ERROR
               GO TO READ-CREDIT-EXIT.
006120     IF WS-STAT1 = "2" OR "3" OR "4"
006130         MOVE 3            TO WS-F-ERROR
006140         PERFORM READ-ERROR.
006150     IF RECORD-LOCKED
	       MOVE W02-CREDIT	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
006160         PERFORM LOCKED-RECORD
006170         GO TO READ-CREDIT-NEXT-LOCK.
           IF TOO-MANY-LOCKS
               UNLOCK CREDIT
               GO TO READ-CREDIT-NEXT-LOCK.
006200       GO TO READ-CREDIT-EXIT.

006080 READ-CREDIT-PREV.
006090	     READ CREDIT PREVIOUS WITH IGNORE LOCK.
006100	   IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
006110         GO TO READ-CREDIT-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
               MOVE 3            TO WS-F-ERROR
               GO TO READ-CREDIT-EXIT.
006120     IF WS-STAT1 = "2" OR "3" OR "4"
006130         MOVE 3            TO WS-F-ERROR
006140         PERFORM READ-ERROR.
006200       GO TO READ-CREDIT-EXIT.

006080 START-AT-CRED-AC.
006090       START CREDIT
               KEY >= P-NUMBER.
006100	   IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
006110         GO TO READ-CREDIT-EXIT.
           IF WS-STATUS = "23"
               MOVE 3            TO WS-F-ERROR
               GO TO READ-CREDIT-EXIT.
006120     IF WS-STAT1 = "2" OR "3" OR "4"
006130         MOVE 3            TO WS-F-ERROR
006140         PERFORM READ-ERROR.
006200       GO TO READ-CREDIT-EXIT.

006080 START-AT-CRED-NAME.
006090       START CREDIT
               KEY >= P-NKEY.
006100	   IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
006110         GO TO READ-CREDIT-EXIT.
           IF WS-STATUS = "23"
               MOVE 3            TO WS-F-ERROR
               GO TO READ-CREDIT-EXIT.
006120     IF WS-STAT1 = "2" OR "3" OR "4"
006130         MOVE 3            TO WS-F-ERROR
006140         PERFORM READ-ERROR.

006520 READ-CREDIT-EXIT.
006530       EXIT.
