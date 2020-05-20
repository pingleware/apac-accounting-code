      *
      *     ***  *     ***** ****    *   *   *    ***** *** *     *****
      *    *   * *       *   *   *  * *  *   *    *      *  *     *
      *    *     *       *   *   * *   * **  *    *      *  *     *
      *    *     *       *   ****  ***** * * *    ***    *  *     ***
      *    *  ** *       *   *   * *   * *  **    *      *  *     *
      *    *   * *       *   *   * *   * *   *    *      *  *     *
      *     ***  *****   *   *   * *   * *   *    *     *** ***** *****
      *
008370 READ-GLTRAN.
008380	     READ GLTRAN
               KEY IS T-KEY.
008390     IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
008400         GO TO READ-GLTRAN-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
008420         MOVE 11           TO WS-F-ERROR
008400         GO TO READ-GLTRAN-EXIT.
008410     IF WS-STAT1 = "2" OR "3" OR "4"
008420         MOVE 11           TO WS-F-ERROR
008430         PERFORM READ-ERROR.
008440     IF RECORD-LOCKED
	       MOVE W02-GLTRAN	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
008450         PERFORM LOCKED-RECORD
008460         GO TO READ-GLTRAN.
008470       MOVE 11             TO WS-F-ERROR.
008480       PERFORM READ-ERROR.
             GO TO READ-GLTRAN-EXIT.

008370 READ-GLTRAN-NEXT.
008380       READ GLTRAN NEXT.
008390     IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
008400         GO TO READ-GLTRAN-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
008420         MOVE 11           TO WS-F-ERROR
008400         GO TO READ-GLTRAN-EXIT.
008410     IF WS-STAT1 = "2" OR "3" OR "4"
008420         MOVE 11           TO WS-F-ERROR
008430         PERFORM READ-ERROR.
008440     IF RECORD-LOCKED
	       MOVE W02-GLTRAN	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
008450         PERFORM LOCKED-RECORD
008460         GO TO READ-GLTRAN-NEXT.
008470       MOVE 11             TO WS-F-ERROR.
008480       PERFORM READ-ERROR.
             GO TO READ-GLTRAN-EXIT.

008370 READ-GLTRAN-PREV.
008380       READ GLTRAN PREVIOUS.
008390     IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
008400         GO TO READ-GLTRAN-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
008420         MOVE 11           TO WS-F-ERROR
008400         GO TO READ-GLTRAN-EXIT.
008410     IF WS-STAT1 = "2" OR "3" OR "4"
008420         MOVE 11           TO WS-F-ERROR
008430         PERFORM READ-ERROR.
008440     IF RECORD-LOCKED
	       MOVE W02-GLTRAN	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
008450         PERFORM LOCKED-RECORD
008460         GO TO READ-GLTRAN-PREV.
008470       MOVE 11             TO WS-F-ERROR.
008480       PERFORM READ-ERROR.
             GO TO READ-GLTRAN-EXIT.

008370 START-AT-GLTRAN-KEY.
008380       START GLTRAN 
               KEY >= T-KEY.
008390     IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
008400         GO TO READ-GLTRAN-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
008420         MOVE 11           TO WS-F-ERROR
008400         GO TO READ-GLTRAN-EXIT.
008410     IF WS-STAT1 = "2" OR "3" OR "4"
008420         MOVE 11           TO WS-F-ERROR
008430         PERFORM READ-ERROR.
             GO TO READ-GLTRAN-EXIT.

008370 START-AT-GLTRAN-AKEY.
008380       START GLTRAN 
               KEY >= T-AKEY.
008390     IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
008400         GO TO READ-GLTRAN-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
008420         MOVE 11           TO WS-F-ERROR
008400         GO TO READ-GLTRAN-EXIT.
008410     IF WS-STAT1 = "2" OR "3" OR "4"
008420         MOVE 11           TO WS-F-ERROR
008430         PERFORM READ-ERROR.
	     GO TO READ-GLTRAN-EXIT.

008370 START-AT-GLTRAN-VKEY.
008380       START GLTRAN 
	       KEY >= T-VKEY.
008390     IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
008400         GO TO READ-GLTRAN-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
008420         MOVE 11           TO WS-F-ERROR
008400         GO TO READ-GLTRAN-EXIT.
008410     IF WS-STAT1 = "2" OR "3" OR "4"
008420         MOVE 11           TO WS-F-ERROR
008430         PERFORM READ-ERROR.

008490 READ-GLTRAN-EXIT.
008500       EXIT.
