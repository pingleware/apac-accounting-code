      *
      *     ***  ****  ***** ****    *	 *   *	  ***** *** *	  *****
      *    *   * *   *   *   *   *  * *  *   *    *      *  *     *
      *    *	 *   *	 *   *	 * *   * **  *	  *	 *  *	  *
      *    *	 ****	 *   ****  ***** * * *	  ***	 *  *	  ***
      *    *	 *   *	 *   *	 * *   * *  **	  *	 *  *	  *
      *    *   * *   *   *   *   * *   * *   *    *      *  *     *
      *     ***  *   *	 *   *	 * *   * *   *	  *	*** ***** *****
      *
005940 READ-CRTRAN.
005950	     READ CTRANS
	       KEY IS CT-KEY.
005960     IF WS-STATUS = "00"
005990         MOVE ZERO         TO WS-F-ERROR
005970         GO TO READ-CRTRAN-EXIT.
           IF WS-STATUS = "23"
005990         MOVE 4            TO WS-F-ERROR
	       GO TO READ-CRTRAN-EXIT.
005980     IF WS-STAT1 = "2" OR "3" OR "4"
005990         MOVE 4            TO WS-F-ERROR
006000         PERFORM READ-ERROR.
006010     IF RECORD-LOCKED
	       MOVE W02-CRTRAN	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
006020	       PERFORM LOCKED-RECORD
006030         GO TO READ-CRTRAN.
006040       MOVE 4              TO WS-F-ERROR.
006050       PERFORM READ-ERROR.

000020 READ-CRTRAN-NEXT.
000030	     READ CTRANS NEXT.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-CRTRAN-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
	       MOVE 4		 TO WS-F-ERROR
000050	       GO TO READ-CRTRAN-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 4		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
000090     IF RECORD-LOCKED
	       MOVE W02-CRTRAN	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000100         PERFORM LOCKED-RECORD
000110	       GO TO READ-CRTRAN-NEXT.
000050	     GO TO READ-CRTRAN-EXIT.

000020 READ-CRTRAN-PREV.
000030	     READ CTRANS PREVIOUS.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-CRTRAN-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
	       MOVE 4		 TO WS-F-ERROR
000050	       GO TO READ-CRTRAN-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 4		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
000090     IF RECORD-LOCKED
	       MOVE W02-CRTRAN	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000100         PERFORM LOCKED-RECORD
000110	       GO TO READ-CRTRAN-PREV.
000050	     GO TO READ-CRTRAN-EXIT.

000020 START-AT-CTRN-KEY.
000030	     START CTRANS
	       KEY >= CT-KEY.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-CRTRAN-EXIT.
           IF WS-STATUS = "23"
	       MOVE 4		 TO WS-F-ERROR
000050	       GO TO READ-CRTRAN-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 4		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
	     GO TO READ-CRTRAN-EXIT.

000020 START-AT-CTRN-CODE.
000030	     START CTRANS
	       KEY >= CT-TRSEQ.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-CRTRAN-EXIT.
           IF WS-STATUS = "23"
	       MOVE 4		 TO WS-F-ERROR
000050	       GO TO READ-CRTRAN-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 4		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
	     GO TO READ-CRTRAN-EXIT.

000020 START-AT-CTRN-PJ.
000030	     START CTRANS
	       KEY >= CT-PJKEY.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-CRTRAN-EXIT.
           IF WS-STATUS = "23"
	       MOVE 4		 TO WS-F-ERROR
000050	       GO TO READ-CRTRAN-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 4		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
	     GO TO READ-CRTRAN-EXIT.

006060 READ-CRTRAN-EXIT.
006070       EXIT.
