      *
      *     ***  *****	***   ***  *   *   ***** ***** *****   *** *   *
      *    *   *   *   *   * *	 * *  *      *	 *   * *	*  *   *
      *    *	   *   *   * *	   * *	     *	 *  *  *	*  **  *
      *     ***    *   *   * *	   **	     *	 ***   ***	*  * * *
      *        *   *   *   * *	   * *	     *	 *  *  *	*  *  **
      *    *   *   *   *   * *	 * *  *      *	 *   * *	*  *   *
      *     ***    *	***   ***  *   *     *	 *   * *       *** *   *
      *
000020 READ-TRFRIN.
000030	     READ TRFRIN WITH IGNORE LOCK
	       KEY IS SIN-KEY.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-TRFRIN-EXIT.
000040     IF WS-STATUS = "23"
000070	       MOVE 62		 TO WS-F-ERROR
000050	       GO TO READ-TRFRIN-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 62		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
000120	     MOVE 62		 TO WS-F-ERROR.
000130       PERFORM READ-ERROR.
000140	     GO TO READ-TRFRIN-EXIT.

000020 READ-TRFRIN-LOCK.
000030	     READ TRFRIN WITH KEPT LOCK
	       KEY IS SIN-KEY.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-TRFRIN-EXIT.
000040     IF WS-STATUS = "23"
000070	       MOVE 62		 TO WS-F-ERROR
000050	       GO TO READ-TRFRIN-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 62		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
006280	   IF RECORD-LOCKED
006290         PERFORM LOCKED-RECORD
006300	       GO TO READ-TRFRIN-LOCK.
006310     IF TOO-MANY-LOCKS
006320	       UNLOCK TRFRIN
006330	       GO TO READ-TRFRIN-LOCK.
000140	     GO TO READ-TRFRIN-EXIT.

006080 READ-TRFRIN-NEXT.
006090	     READ TRFRIN NEXT.
006100     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110	       GO TO READ-TRFRIN-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
	       MOVE 62		 TO WS-F-ERROR
	       GO TO READ-TRFRIN-EXIT.
006120     IF WS-STAT1 = "2" OR "3" OR "4"
006130	       MOVE 62		 TO WS-F-ERROR
006140         PERFORM READ-ERROR.
006150     IF RECORD-LOCKED
006160         PERFORM LOCKED-RECORD
006170	       GO TO READ-TRFRIN-NEXT.
006200	     GO TO READ-TRFRIN-EXIT.

006080 READ-TRFRIN-NEXT-LOCK.
006090	     READ TRFRIN NEXT WITH KEPT LOCK.
006100     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110	       GO TO READ-TRFRIN-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
	       MOVE 62		 TO WS-F-ERROR
	       GO TO READ-TRFRIN-EXIT.
006120     IF WS-STAT1 = "2" OR "3" OR "4"
006130	       MOVE 62		 TO WS-F-ERROR
006140         PERFORM READ-ERROR.
006150     IF RECORD-LOCKED
006160         PERFORM LOCKED-RECORD
006170	       GO TO READ-TRFRIN-NEXT-LOCK.
           IF TOO-MANY-LOCKS
	       UNLOCK TRFRIN
	       GO TO READ-TRFRIN-NEXT-LOCK.
006200	     GO TO READ-TRFRIN-EXIT.

006080 READ-TRFRIN-PREV.
006090	     READ TRFRIN PREVIOUS.
006100     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110	       GO TO READ-TRFRIN-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
	       MOVE 62		 TO WS-F-ERROR
	       GO TO READ-TRFRIN-EXIT.
006120     IF WS-STAT1 = "2" OR "3" OR "4"
006130	       MOVE 62		 TO WS-F-ERROR
006140         PERFORM READ-ERROR.
006150     IF RECORD-LOCKED
006160         PERFORM LOCKED-RECORD
006170	       GO TO READ-TRFRIN-PREV.
006200	     GO TO READ-TRFRIN-EXIT.

006080 START-AT-TFIN-KEY.
006090	     START TRFRIN
	       KEY >= SIN-KEY.
006100     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110	       GO TO READ-TRFRIN-EXIT.
           IF WS-STATUS = "23"
	       MOVE 62		 TO WS-F-ERROR
	       GO TO READ-TRFRIN-EXIT.
006120     IF WS-STAT1 = "2" OR "3" OR "4"
006130	       MOVE 62		 TO WS-F-ERROR
006140         PERFORM READ-ERROR.
006200	     GO TO READ-TRFRIN-EXIT.

006080 START-AT-TFIN-DATE.
006090	     START TRFRIN
	       KEY >= SIN-DKEY.
006100     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110	       GO TO READ-TRFRIN-EXIT.
           IF WS-STATUS = "23"
	       MOVE 62		 TO WS-F-ERROR
	       GO TO READ-TRFRIN-EXIT.
006120     IF WS-STAT1 = "2" OR "3" OR "4"
006130	       MOVE 62		 TO WS-F-ERROR
006140         PERFORM READ-ERROR.

000150 READ-TRFRIN-EXIT.
000160       EXIT.
