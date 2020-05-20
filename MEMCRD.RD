      *
      *    *   * ***** *   *   ***  ****  ****	    ***** ***** *     *****
      *    ** ** *     ** **  *   * *	* *   *     *	    *	*     *
      *    * * * *     * * *  *     *	* *   *     *	    *	*     *
      *    *   * ***   *   *  *     ****  *   *     ***	    *	*     ***
      *    *   * *     *   *  *     *	* *   *     *	    *	*     *
      *    *   * *     *   *  *   * *	* *   *     *	    *	*     *
      *    *   * ***** *   *   ***  *	* ****	    *	  ***** ***** *****
      *
009060 READ-MEMCRD.
009070	     READ MEMCRD	 WITH IGNORE LOCK
	       KEY IS MEM-KEY.
009080     IF WS-STATUS = "00"
006890         MOVE ZERO         TO WS-F-ERROR
009090	       GO TO READ-MEMCRD-EXIT.
009900	   IF WS-STATUS = "23"
006890	       MOVE 62		 TO WS-F-ERROR
006870	       GO TO READ-MEMCRD-EXIT.
009100	   IF WS-STAT1 = "2" OR "3" OR "4"
009110	       MOVE 62		 TO WS-F-ERROR
009120         PERFORM READ-ERROR.
009160	     MOVE 62		 TO WS-F-ERROR.
	     GO TO READ-MEMCRD-EXIT.

006840 READ-MEMCRD-NEXT.
006850	     READ MEMCRD NEXT	 WITH IGNORE LOCK.
006860     IF WS-STATUS = "00"
006890         MOVE ZERO         TO WS-F-ERROR
006870	       GO TO READ-MEMCRD-EXIT.
009900     IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
006890	       MOVE 62		 TO WS-F-ERROR
006870	       GO TO READ-MEMCRD-EXIT.
006880     IF WS-STAT1 = "2" OR "3" OR "4"
006890	       MOVE 62		 TO WS-F-ERROR
006900         PERFORM READ-ERROR.
	     GO TO READ-MEMCRD-EXIT.

006840 START-AT-DEBTAC.
006850	     START MEMCRD
	       KEY >= MEM-KEY.
006860     IF WS-STATUS = "00"
006890         MOVE ZERO         TO WS-F-ERROR
006870	       GO TO READ-MEMCRD-EXIT.
009900     IF WS-STATUS = "23"
006890	       MOVE 62		 TO WS-F-ERROR
006870	       GO TO READ-MEMCRD-EXIT.
006880     IF WS-STAT1 = "2" OR "3" OR "4"
006890	       MOVE 62		 TO WS-F-ERROR
006900         PERFORM READ-ERROR.

009180 READ-MEMCRD-EXIT.
009190       EXIT.
