      *
      *    ****  ***** ****    *   ****  *****	   ***** *** *	   *****
      *    *   * *     *   *  * *  *   *   *	   *	  *  *	   *
      *    *   * *     *   * *	 * *   *   *	   *	  *  *	   *
      *    *   * ***   ****  ***** ****    *	   ***	  *  *	   ***
      *    *   * *     *   * *	 * *  *    *	   *	  *  *	   *
      *    *   * *     *   * *	 * *   *   *	   *	  *  *	   *
      *    ****  ***** *   * *	 * *   *   *	   *	 *** ***** *****
      *
008650 READ-DEPART.
008660	     READ DEPART	 WITH IGNORE LOCK
	       KEY IS DPT-CODE.
008670     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
008680         GO TO READ-DEPART-EXIT.
	   IF WS-STATUS = "23"
	       MOVE 7		 TO WS-F-ERROR
	       GO TO READ-DEPART-EXIT.
008690	   IF WS-STAT1 = "2" OR "3" OR "4"
008700         MOVE 7            TO WS-F-ERROR
008710         PERFORM READ-ERROR.
008770       GO TO READ-DEPART-EXIT.
      
008780 READ-DEPART-LOCK.
008790	     READ DEPART	 WITH KEPT LOCK
	       KEY IS DPT-CODE.
008800	   IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
008810         GO TO READ-DEPART-EXIT.
	   IF WS-STATUS = "23"
	       MOVE 7		 TO WS-F-ERROR
	       GO TO READ-DEPART-EXIT.
008820	   IF WS-STAT1 = "2" OR "3" OR "4"
008830         MOVE 7            TO WS-F-ERROR
008840         PERFORM READ-ERROR.
008850     IF RECORD-LOCKED
008860	       PERFORM LOCKED-RECORD
008870         GO TO READ-DEPART-LOCK.
008880     IF TOO-MANY-LOCKS
008890         UNLOCK DEPART
008900         GO TO READ-DEPART-LOCK.
008770       GO TO READ-DEPART-EXIT.
      
006080 READ-DEPART-NEXT.
006090	     READ DEPART NEXT.
006100     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110	       GO TO READ-DEPART-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
	       MOVE 7		 TO WS-F-ERROR
	       GO TO READ-DEPART-EXIT.
006120     IF WS-STAT1 = "2" OR "3" OR "4"
006130	       MOVE 7		 TO WS-F-ERROR
006140         PERFORM READ-ERROR.
006150     IF RECORD-LOCKED
006160         PERFORM LOCKED-RECORD
006170	       GO TO READ-DEPART-NEXT.
006200	     GO TO READ-DEPART-EXIT.

006080 READ-DEPART-NEXT-LOCK.
006090	     READ DEPART NEXT WITH KEPT LOCK.
006100     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110	       GO TO READ-DEPART-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
	       MOVE 7		 TO WS-F-ERROR
	       GO TO READ-DEPART-EXIT.
006120     IF WS-STAT1 = "2" OR "3" OR "4"
006130	       MOVE 7		 TO WS-F-ERROR
006140         PERFORM READ-ERROR.
006150     IF RECORD-LOCKED
006160         PERFORM LOCKED-RECORD
006170	       GO TO READ-DEPART-NEXT-LOCK.
           IF TOO-MANY-LOCKS
	       UNLOCK DEPART
	       GO TO READ-DEPART-NEXT-LOCK.
006200	     GO TO READ-DEPART-EXIT.

006080 READ-DEPART-PREV.
006090	     READ DEPART PREVIOUS.
006100     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110	       GO TO READ-DEPART-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
	       MOVE 7		 TO WS-F-ERROR
	       GO TO READ-DEPART-EXIT.
006120     IF WS-STAT1 = "2" OR "3" OR "4"
006130	       MOVE 7		 TO WS-F-ERROR
006140         PERFORM READ-ERROR.
006150     IF RECORD-LOCKED
006160         PERFORM LOCKED-RECORD
006170	       GO TO READ-DEPART-PREV.
006200	     GO TO READ-DEPART-EXIT.

006080 START-AT-DEPT-CODE.
006090	     START DEPART
	       KEY >= DPT-CODE.
006100     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110	       GO TO READ-DEPART-EXIT.
           IF WS-STATUS = "23"
	       MOVE 7		 TO WS-F-ERROR
	       GO TO READ-DEPART-EXIT.
006120     IF WS-STAT1 = "2" OR "3" OR "4"
006130	       MOVE 7		 TO WS-F-ERROR
006140         PERFORM READ-ERROR.
006200	     GO TO READ-DEPART-EXIT.

006080 START-AT-DEPT-NAME.
006090	     START DEPART
	       KEY >= DPT-NKEY.
006100     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110	       GO TO READ-DEPART-EXIT.
           IF WS-STATUS = "23"
	       MOVE 7		 TO WS-F-ERROR
	       GO TO READ-DEPART-EXIT.
006120     IF WS-STAT1 = "2" OR "3" OR "4"
006130	       MOVE 7		 TO WS-F-ERROR
006140         PERFORM READ-ERROR.

008930 READ-DEPART-EXIT.
008940       EXIT.
