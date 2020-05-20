      *
      *     ***  *     ***** ****    *   *   *    ***** *** *     *****
      *    *   * *       *   *   *  * *  *   *    *      *  *     *
      *    *     *       *   *   * *   * **  *    *      *  *     *
      *    *     *       *   ****  ***** * * *    ***    *  *     ***
      *    *  ** *       *   *   * *   * *  **    *      *  *     *
      *    *   * *       *   *   * *   * *   *    *      *  *     *
      *     ***  *****   *   *   * *   * *   *    *     *** ***** *****
      *
008370 READ-GLTRANP.
008380	     READ GLTRANP
	       KEY IS TP-KEY.
008390     IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
008400	       GO TO READ-GLTRANP-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
008420	       MOVE 61		 TO WS-F-ERROR
008400	       GO TO READ-GLTRANP-EXIT.
008410     IF WS-STAT1 = "2" OR "3" OR "4"
008420	       MOVE 61		 TO WS-F-ERROR
008430         PERFORM READ-ERROR.
008440     IF RECORD-LOCKED
	       MOVE W02-GLTRANP	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
008450         PERFORM LOCKED-RECORD
008460	       GO TO READ-GLTRANP.
008470	     MOVE 61		 TO WS-F-ERROR.
008480       PERFORM READ-ERROR.
	     GO TO READ-GLTRANP-EXIT.

008370 READ-GLTRANP-NEXT.
008380	     READ GLTRANP NEXT.
008390     IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
008400	       GO TO READ-GLTRANP-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
008420	       MOVE 61		 TO WS-F-ERROR
008400	       GO TO READ-GLTRANP-EXIT.
008410     IF WS-STAT1 = "2" OR "3" OR "4"
008420	       MOVE 61		 TO WS-F-ERROR
008430         PERFORM READ-ERROR.
008440     IF RECORD-LOCKED
	       MOVE W02-GLTRANP	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
008450         PERFORM LOCKED-RECORD
008460	       GO TO READ-GLTRANP-NEXT.
008470	     MOVE 61		 TO WS-F-ERROR.
008480       PERFORM READ-ERROR.
	     GO TO READ-GLTRANP-EXIT.

008370 READ-GLTRANP-PREV.
008380	     READ GLTRANP PREVIOUS.
008390     IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
008400	       GO TO READ-GLTRANP-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
008420	       MOVE 61		 TO WS-F-ERROR
008400	       GO TO READ-GLTRANP-EXIT.
008410     IF WS-STAT1 = "2" OR "3" OR "4"
008420	       MOVE 61		 TO WS-F-ERROR
008430         PERFORM READ-ERROR.
008440     IF RECORD-LOCKED
	       MOVE W02-GLTRANP	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
008450         PERFORM LOCKED-RECORD
008460	       GO TO READ-GLTRANP-PREV.
008470	     MOVE 61		 TO WS-F-ERROR.
008480       PERFORM READ-ERROR.
	     GO TO READ-GLTRANP-EXIT.

008370 START-AT-GLTRANP-KEY.
008380	     START GLTRANP
	       KEY >= TP-KEY.
008390     IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
008400	       GO TO READ-GLTRANP-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
008420	       MOVE 61		 TO WS-F-ERROR
008400	       GO TO READ-GLTRANP-EXIT.
008410     IF WS-STAT1 = "2" OR "3" OR "4"
008420	       MOVE 61		 TO WS-F-ERROR
008430         PERFORM READ-ERROR.
	     GO TO READ-GLTRANP-EXIT.

008370 START-AT-GLTRANP-AKEY.
008380	     START GLTRANP
	       KEY >= TP-AKEY.
008390     IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
008400	       GO TO READ-GLTRANP-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
008420	       MOVE 61		 TO WS-F-ERROR
008400	       GO TO READ-GLTRANP-EXIT.
008410     IF WS-STAT1 = "2" OR "3" OR "4"
008420	       MOVE 61		 TO WS-F-ERROR
008430         PERFORM READ-ERROR.
	     GO TO READ-GLTRANP-EXIT.

008370 START-AT-GLTRANP-VKEY.
008380	     START GLTRANP
	       KEY >= TP-VKEY.
008390     IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
008400	       GO TO READ-GLTRANP-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
008420	       MOVE 61		 TO WS-F-ERROR
008400	       GO TO READ-GLTRANP-EXIT.
008410     IF WS-STAT1 = "2" OR "3" OR "4"
008420	       MOVE 61		 TO WS-F-ERROR
008430         PERFORM READ-ERROR.

008490 READ-GLTRANP-EXIT.
008500       EXIT.
