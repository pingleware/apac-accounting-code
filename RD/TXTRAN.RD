      *
      *  ***** *   * ***** ****    *   *   *     **** *** *    ****  
      *    *   *   *   *   *   *  * *  *   *     *     *  *    *     
      *    *    * *    *   *   * *   * **  *     *     *  *    *     
      *    *     *     *   ****  ***** * * *     ***   *  *    ***   
      *    *    * *    *   *   * *   * *  **     *     *  *    *     
      *    *   *   *   *   *   * *   * *   *     *     *  *    *     
      *    *   *   *   *   *   * *   * *   *     *    *** **** ****  
      *
009860 READ-TXTRAN.
009870	     READ TXTRAN	 WITH IGNORE LOCK
               KEY IS TAX-KEY.                
009880     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
009890         GO TO READ-TXTRAN-EXIT.
           IF WS-STATUS = 23
               MOVE 32           TO WS-F-ERROR
               GO TO READ-TXTRAN-EXIT.
009900     IF WS-STAT1 = "2" OR "3" OR "4"
009910         MOVE 32           TO WS-F-ERROR
009920         PERFORM READ-ERROR.
009980       GO TO READ-TXTRAN-EXIT.
      
009990 READ-TXTRAN-LOCK.
010000       READ TXTRAN WITH KEPT LOCK
               KEY IS TAX-KEY.
010010     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
010020         GO TO READ-TXTRAN-EXIT.
           IF WS-STATUS = 23
               MOVE 32           TO WS-F-ERROR
               GO TO READ-TXTRAN-EXIT.
010030     IF WS-STAT1 = "2" OR "3" OR "4"
010080         MOVE 32       TO WS-F-ERROR
010090         PERFORM READ-ERROR.
010100     IF RECORD-LOCKED
	       MOVE W02-TXTRAN	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
010110         PERFORM LOCKED-RECORD
010120         GO TO READ-TXTRAN-LOCK.
008420     IF TOO-MANY-LOCKS
008430         UNLOCK TXTRAN
008440         GO TO READ-TXTRAN-LOCK.
009980       GO TO READ-TXTRAN-EXIT.

008190 READ-TXTRAN-NEXT.
008200	     READ TXTRAN NEXT	 WITH IGNORE LOCK.
008210     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
008220         GO TO READ-TXTRAN-EXIT.
009900     IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
009910         MOVE 32           TO WS-F-ERROR
009920         GO TO READ-TXTRAN-EXIT.
008230     IF WS-STAT1 = "2" OR "3" OR "4"
008240         MOVE 32           TO WS-F-ERROR
008250         PERFORM READ-ERROR.
008310       GO TO READ-TXTRAN-EXIT.

008190 READ-TXTRAN-NEXT-LOCK.
008200       READ TXTRAN NEXT WITH KEPT LOCK.
008210     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
008220         GO TO READ-TXTRAN-EXIT.
009900     IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
009910         MOVE 32           TO WS-F-ERROR
009920         GO TO READ-TXTRAN-EXIT.
008230     IF WS-STAT1 = "2" OR "3" OR "4"
008240         MOVE 32           TO WS-F-ERROR
008250         PERFORM READ-ERROR.
008260     IF RECORD-LOCKED
	       MOVE W02-TXTRAN	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
008270         PERFORM LOCKED-RECORD
008280         GO TO READ-TXTRAN-NEXT-LOCK.
008420     IF TOO-MANY-LOCKS
008430         UNLOCK TXTRAN
008440         GO TO READ-TXTRAN-NEXT-LOCK.
008310       GO TO READ-TXTRAN-EXIT.

008190 START-AT-TXTRAN-KEY.
008200       START TXTRAN 
               KEY >= TAX-KEY.
008210     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
008220         GO TO READ-TXTRAN-EXIT.
009900     IF WS-STATUS = "23"
009910         MOVE 32           TO WS-F-ERROR
009920         GO TO READ-TXTRAN-EXIT.
008230     IF WS-STAT1 = "2" OR "3" OR "4"
008240         MOVE 32           TO WS-F-ERROR
008250         PERFORM READ-ERROR.

010150 READ-TXTRAN-EXIT.
010160       EXIT.
