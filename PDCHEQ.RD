      *
      *    ****  ****	***  *	 * *****  ***	  ***** *** *	  *****
      *    *   * *   * *   * *	 * *	 *   *	  *	 *  *	  *
      *    *   * *   * *     *	 * *	 *   *	  *	 *  *	  *
      *    ****  *   * *     ***** ***	 *   *	  ***	 *  *	  ***
      *    *	 *   * *     *	 * *	 * * *	  *	 *  *	  *
      *    *	 *   * *   * *	 * *	  ***	  *	 *  *	  *
      *    *	 ****	***  *	 * *****     *	  *	*** ***** *****
      *
008370 READ-PDCHEQ.
008380	     READ PDCHEQ
	       KEY IS PDT-KEY.
008390     IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
008400	       GO TO READ-PDCHEQ-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
008420	       MOVE 44		 TO WS-F-ERROR
008400	       GO TO READ-PDCHEQ-EXIT.
008410     IF WS-STAT1 = "2" OR "3" OR "4"
008420	       MOVE 44		 TO WS-F-ERROR
008430         PERFORM READ-ERROR.
008440     IF RECORD-LOCKED
	       MOVE W02-PDCHEQ	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
008450         PERFORM LOCKED-RECORD
008460	       GO TO READ-PDCHEQ.
008470	     MOVE 44		 TO WS-F-ERROR.
008480       PERFORM READ-ERROR.
	     GO TO READ-PDCHEQ-EXIT.

008370 READ-PDCHEQ-NEXT.
008380	     READ PDCHEQ NEXT.
008390     IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
008400	       GO TO READ-PDCHEQ-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
008420	       MOVE 44		 TO WS-F-ERROR
008400	       GO TO READ-PDCHEQ-EXIT.
008410     IF WS-STAT1 = "2" OR "3" OR "4"
008420	       MOVE 44		 TO WS-F-ERROR
008430         PERFORM READ-ERROR.
008440     IF RECORD-LOCKED
	       MOVE W02-PDCHEQ	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
008450         PERFORM LOCKED-RECORD
008460	       GO TO READ-PDCHEQ-NEXT.
008470	     MOVE 44		 TO WS-F-ERROR.
008480       PERFORM READ-ERROR.
	     GO TO READ-PDCHEQ-EXIT.

008370 READ-PDCHEQ-PREV.
008380	     READ PDCHEQ PREVIOUS.
008390     IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
008400	       GO TO READ-PDCHEQ-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
008420	       MOVE 44		 TO WS-F-ERROR
008400	       GO TO READ-PDCHEQ-EXIT.
008410     IF WS-STAT1 = "2" OR "3" OR "4"
008420	       MOVE 44		 TO WS-F-ERROR
008430         PERFORM READ-ERROR.
008440     IF RECORD-LOCKED
	       MOVE W02-PDCHEQ	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
008450         PERFORM LOCKED-RECORD
008460	       GO TO READ-PDCHEQ-PREV.
008470	     MOVE 44		 TO WS-F-ERROR.
008480       PERFORM READ-ERROR.
	     GO TO READ-PDCHEQ-EXIT.

008370 START-AT-PDCHEQ-KEY.
008380	     START PDCHEQ
	       KEY >= PDT-KEY.
008390     IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
008400	       GO TO READ-PDCHEQ-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
008420	       MOVE 44		 TO WS-F-ERROR
008400	       GO TO READ-PDCHEQ-EXIT.
008410     IF WS-STAT1 = "2" OR "3" OR "4"
008420	       MOVE 44		 TO WS-F-ERROR
008430         PERFORM READ-ERROR.
	     GO TO READ-PDCHEQ-EXIT.

008370 START-AT-PDCHEQ-AC.
008380	     START PDCHEQ
	       KEY >= PDT-AC.
008390     IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
008400	       GO TO READ-PDCHEQ-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
008420	       MOVE 44		 TO WS-F-ERROR
008400	       GO TO READ-PDCHEQ-EXIT.
008410     IF WS-STAT1 = "2" OR "3" OR "4"
008420	       MOVE 44		 TO WS-F-ERROR
008430         PERFORM READ-ERROR.

008370 START-AT-PDCHEQ-REF.
008380	     START PDCHEQ
	       KEY >= PDT-REFER.
008390     IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
008400	       GO TO READ-PDCHEQ-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
008420	       MOVE 44		 TO WS-F-ERROR
008400	       GO TO READ-PDCHEQ-EXIT.
008410     IF WS-STAT1 = "2" OR "3" OR "4"
008420	       MOVE 44		 TO WS-F-ERROR
008430         PERFORM READ-ERROR.

008490 READ-PDCHEQ-EXIT.
008500       EXIT.
