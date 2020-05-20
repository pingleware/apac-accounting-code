      *
      *  *****  ***   ***   ***  *     *****     **** *** *    ****  
      *  *     *   * *   * *   * *     *         *     *  *    *     
      *  *     *     *     *     *     *         *     *  *    *     
      *  ***   *     *      ***  *     ***       ***   *  *    ***   
      *  *     *  ** *  **     * *     *         *     *  *    *     
      *  *     *   * *   * *   * *     *         *     *  *    *     
      *  *****  ***   ***   ***  ***** *****     *    *** **** ****  
      *
009860 READ-EGGSLE.
009870       READ EGGSLE 
               KEY IS EGG-KEY.                
009880     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
009890         GO TO READ-EGGSLE-EXIT.
           IF WS-STATUS = 23
               MOVE 30          TO WS-F-ERROR
               GO TO READ-EGGSLE-EXIT.
009900     IF WS-STAT1 = "2" OR "3" OR "4"
009910         MOVE 30          TO WS-F-ERROR
009920         PERFORM READ-ERROR.
009930     IF RECORD-LOCKED
009940         PERFORM LOCKED-RECORD
009950         GO TO READ-EGGSLE.
009980       GO TO READ-EGGSLE-EXIT.
      
009990 READ-EGGSLE-LOCK.
010000       READ EGGSLE WITH KEPT LOCK
               KEY IS EGG-KEY.
010010     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
010020         GO TO READ-EGGSLE-EXIT.
           IF WS-STATUS = 23
               MOVE 30          TO WS-F-ERROR
               GO TO READ-EGGSLE-EXIT.
010030     IF WS-STAT1 = "2" OR "3" OR "4"
010080         MOVE 30      TO WS-F-ERROR
010090         PERFORM READ-ERROR.
010100     IF RECORD-LOCKED
010110         PERFORM LOCKED-RECORD
010120         GO TO READ-EGGSLE-LOCK.
008420     IF TOO-MANY-LOCKS
008430         UNLOCK EGGSLE
008440         GO TO READ-EGGSLE-LOCK.
009980       GO TO READ-EGGSLE-EXIT.

008190 READ-EGGSLE-NEXT.
008200       READ EGGSLE NEXT.
008210     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
008220         GO TO READ-EGGSLE-EXIT.
009900     IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
009910         MOVE 30          TO WS-F-ERROR
009920         GO TO READ-EGGSLE-EXIT.
008230     IF WS-STAT1 = "2" OR "3" OR "4"
008240         MOVE 30          TO WS-F-ERROR
008250         PERFORM READ-ERROR.
008260     IF RECORD-LOCKED
008270         PERFORM LOCKED-RECORD
008280         GO TO READ-EGGSLE-NEXT.
008310       GO TO READ-EGGSLE-EXIT.

008190 READ-EGGSLE-NEXT-LOCK.
008200       READ EGGSLE NEXT WITH KEPT LOCK.
008210     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
008220         GO TO READ-EGGSLE-EXIT.
009900     IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
009910         MOVE 30          TO WS-F-ERROR
009920         GO TO READ-EGGSLE-EXIT.
008230     IF WS-STAT1 = "2" OR "3" OR "4"
008240         MOVE 30          TO WS-F-ERROR
008250         PERFORM READ-ERROR.
008260     IF RECORD-LOCKED
008270         PERFORM LOCKED-RECORD
008280         GO TO READ-EGGSLE-NEXT-LOCK.
008420     IF TOO-MANY-LOCKS
008430         UNLOCK EGGSLE
008440         GO TO READ-EGGSLE-NEXT-LOCK.
008310       GO TO READ-EGGSLE-EXIT.

008190 START-AT-EGGSLE-KEY.
008200       START EGGSLE 
               KEY >= EGG-KEY.
008210     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
008220         GO TO READ-EGGSLE-EXIT.
009900     IF WS-STATUS = "23"
009910         MOVE 30          TO WS-F-ERROR
009920         GO TO READ-EGGSLE-EXIT.
008230     IF WS-STAT1 = "2" OR "3" OR "4"
008240         MOVE 30          TO WS-F-ERROR
008250         PERFORM READ-ERROR.

010150 READ-EGGSLE-EXIT.
010160       EXIT.

