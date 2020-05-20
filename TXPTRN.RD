      *
      *  ***** *   * ****  ***** ****  *   *	 **** *** *    ****
      *    *   *   * *	 *   *	 *   * *   *	 *     *  *    *
      *    *	* *  *	 *   *	 *   * **  *	 *     *  *    *
      *    *	 *   ****    *	 ****  * * *	 ***   *  *    ***
      *    *	* *  *	     *	 *   * *  **	 *     *  *    *
      *    *   *   * *	     *	 *   * *   *	 *     *  *    *
      *    *   *   * *	     *	 *   * *   *	 *    *** **** ****
      *
009860 READ-TXPTRN.
009870	     READ TXPTRN	 WITH IGNORE LOCK
	       KEY IS TXP-KEY.
009880     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
009890	       GO TO READ-TXPTRN-EXIT.
           IF WS-STATUS = 23
	       MOVE 35		 TO WS-F-ERROR
	       GO TO READ-TXPTRN-EXIT.
009900     IF WS-STAT1 = "2" OR "3" OR "4"
009910	       MOVE 35		 TO WS-F-ERROR
009920         PERFORM READ-ERROR.
009980	     GO TO READ-TXPTRN-EXIT.
      
009990 READ-TXPTRN-LOCK.
010000	     READ TXPTRN WITH KEPT LOCK
	       KEY IS TXP-KEY.
010010     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
010020	       GO TO READ-TXPTRN-EXIT.
           IF WS-STATUS = 23
	       MOVE 35		 TO WS-F-ERROR
	       GO TO READ-TXPTRN-EXIT.
010030     IF WS-STAT1 = "2" OR "3" OR "4"
010080	       MOVE 35	     TO WS-F-ERROR
010090         PERFORM READ-ERROR.
010100     IF RECORD-LOCKED
	       MOVE W02-TXPTRN	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
010110         PERFORM LOCKED-RECORD
010120	       GO TO READ-TXPTRN-LOCK.
008420     IF TOO-MANY-LOCKS
008430	       UNLOCK TXPTRN
008440	       GO TO READ-TXPTRN-LOCK.
009980	     GO TO READ-TXPTRN-EXIT.

008190 READ-TXPTRN-NEXT.
008200	     READ TXPTRN NEXT	 WITH IGNORE LOCK.
008210     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
008220	       GO TO READ-TXPTRN-EXIT.
009900     IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
009910	       MOVE 35		 TO WS-F-ERROR
009920	       GO TO READ-TXPTRN-EXIT.
008230     IF WS-STAT1 = "2" OR "3" OR "4"
008240	       MOVE 35		 TO WS-F-ERROR
008250         PERFORM READ-ERROR.
008310	     GO TO READ-TXPTRN-EXIT.

008190 READ-TXPTRN-NEXT-LOCK.
008200	     READ TXPTRN NEXT WITH KEPT LOCK.
008210     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
008220	       GO TO READ-TXPTRN-EXIT.
009900     IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
009910	       MOVE 35		 TO WS-F-ERROR
009920	       GO TO READ-TXPTRN-EXIT.
008230     IF WS-STAT1 = "2" OR "3" OR "4"
008240	       MOVE 35		 TO WS-F-ERROR
008250         PERFORM READ-ERROR.
008260     IF RECORD-LOCKED
	       MOVE W02-TXPTRN	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
008270         PERFORM LOCKED-RECORD
008280	       GO TO READ-TXPTRN-NEXT-LOCK.
008420     IF TOO-MANY-LOCKS
008430	       UNLOCK TXPTRN
008440	       GO TO READ-TXPTRN-NEXT-LOCK.
008310	     GO TO READ-TXPTRN-EXIT.

008190 START-AT-TXPTRN-KEY.
008200	     START TXPTRN
	       KEY >= TXP-KEY.
008210     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
008220	       GO TO READ-TXPTRN-EXIT.
009900     IF WS-STATUS = "23"
009910	       MOVE 35		 TO WS-F-ERROR
009920	       GO TO READ-TXPTRN-EXIT.
008230     IF WS-STAT1 = "2" OR "3" OR "4"
008240	       MOVE 35		 TO WS-F-ERROR
008250         PERFORM READ-ERROR.

010150 READ-TXPTRN-EXIT.
010160       EXIT.
