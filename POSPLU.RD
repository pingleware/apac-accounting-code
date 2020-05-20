      *
      *   ****	 ***   ***  ****  *	*   *	 ***** *** *	 *****
      *   *   * *   * *   * *	* *	*   *	 *	*  *	 *
      *   *   * *   * *     *	* *	*   *	 *	*  *	 *
      *   ****	*   *  ***  ****  *	*   *	 ***	*  *	 ***
      *   *	*   *	  * *	  *	*   *	 *	*  *	 *
      *   *	*   * *   * *	  *	*   *	 *	*  *	 *
      *   *	 ***   ***  *	  *****  ***	 *     *** ***** *****
      *
006080 READ-POSPLU.
	     READ POSPLU WITH IGNORE LOCK
		  KEY IS PLU-KEY.
006100	   IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110	       GO TO READ-POSPLU-EXIT.
           IF WS-STATUS = "23"
	       MOVE 49		 TO WS-F-ERROR
	       GO TO READ-POSPLU-EXIT.
006120     IF WS-STAT1 = "2" OR "3" OR "4"
006130         MOVE 3            TO WS-F-ERROR
006140         PERFORM READ-ERROR.
	     GO TO READ-POSPLU-EXIT.

006210 READ-POSPLU-LOCK.
006220	     READ POSPLU WITH KEPT LOCK
		  KEY IS PLU-KEY.
006230     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006240	       GO TO READ-POSPLU-EXIT.
           IF WS-STATUS = "23"
	       MOVE 49		 TO WS-F-ERROR
	       GO TO READ-POSPLU-EXIT.
006250     IF WS-STAT1 = "2" OR "3" OR "4"
006260	       MOVE 49		 TO WS-F-ERROR
006270         PERFORM READ-ERROR.
006280     IF RECORD-LOCKED
006290         PERFORM LOCKED-RECORD
006300	       GO TO READ-POSPLU-LOCK.
006310     IF TOO-MANY-LOCKS
006320	       UNLOCK POSPLU
006330	       GO TO READ-POSPLU-LOCK.
	     GO TO READ-POSPLU-EXIT.

000310 READ-POSPLU-NEXT.
000320	     READ POSPLU NEXT WITH IGNORE LOCK.
000330     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000340	       GO TO READ-POSPLU-EXIT.
000350     IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
000360	       MOVE 49		 TO WS-F-ERROR
000370	       GO TO READ-POSPLU-EXIT.
000380     IF WS-STAT1 = "2" OR "3" OR "4"
000390	       MOVE 49		 TO WS-F-ERROR
000400         PERFORM READ-ERROR.
	     GO TO READ-POSPLU-EXIT.

000020 START-AT-POS-KEY.
000030	     START POSPLU
	       KEY >= PLU-KEY.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-POSPLU-EXIT.
           IF WS-STATUS = "23"
	       MOVE 49		 TO WS-F-ERROR
000050	       GO TO READ-POSPLU-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 49		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
	     GO TO READ-POSPLU-EXIT.

000020 START-AT-POS-PLU.
000030	     START POSPLU
	       KEY >= PLU-KEY2.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-POSPLU-EXIT.
           IF WS-STATUS = "23"
	       MOVE 49		 TO WS-F-ERROR
000050	       GO TO READ-POSPLU-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 49		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.

006520 READ-POSPLU-EXIT.
006530       EXIT.
