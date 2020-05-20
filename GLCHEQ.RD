      *
      *     ***  *     ****  *****  ***  *   *    ***** *** *     *****
      *    *   * *     *   *   *   *   * *   *    *      *  *     *
      *    *     *     *   *   *   *     *   *    *      *  *     *
      *    *     *     ****    *   *     *****    ***    *  *     ***
      *    *  ** *     *   *   *   *     *   *    *      *  *     *
      *    *   * *     *   *   *   *   * *   *    *      *  *     *
      *     ***  ***** ****    *    ***  *   *    *     *** ***** *****
      *
000020 READ-GLBTCH.
000030       READ GLBTCH
               KEY IS GLB-KEY.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050         GO TO READ-GLBTCH-EXIT.
000040     IF WS-STATUS = "23"
               MOVE 41           TO WS-F-ERROR
000050         GO TO READ-GLBTCH-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070         MOVE 41           TO WS-F-ERROR
000080         PERFORM READ-ERROR.
000090     IF RECORD-LOCKED
	       MOVE W02-GLCHEQ	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000100         PERFORM LOCKED-RECORD
000110         GO TO READ-GLBTCH.
             GO TO READ-GLBTCH-EXIT.

000020 READ-GLBTCH-NEXT.
000030       READ GLBTCH NEXT.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050         GO TO READ-GLBTCH-EXIT.
000040     IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
               MOVE 41           TO WS-F-ERROR
000050         GO TO READ-GLBTCH-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070         MOVE 41           TO WS-F-ERROR
000080         PERFORM READ-ERROR.
000090     IF RECORD-LOCKED
	       MOVE W02-GLCHEQ	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000100         PERFORM LOCKED-RECORD
000110         GO TO READ-GLBTCH-NEXT.
             GO TO READ-GLBTCH-EXIT.

000020 READ-GLBTCH-PREV.
000030       READ GLBTCH PREVIOUS.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050         GO TO READ-GLBTCH-EXIT.
000040     IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
               MOVE 41           TO WS-F-ERROR
000050         GO TO READ-GLBTCH-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070         MOVE 41           TO WS-F-ERROR
000080         PERFORM READ-ERROR.
000090     IF RECORD-LOCKED
	       MOVE W02-GLCHEQ	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000100         PERFORM LOCKED-RECORD
000110         GO TO READ-GLBTCH-PREV.
             GO TO READ-GLBTCH-EXIT.

       START-AT-GLBTCH-KEY.
000030       START GLBTCH
               KEY >= GLB-KEY.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050         GO TO READ-GLBTCH-EXIT.
000040     IF WS-STATUS = "23"
               MOVE 41           TO WS-F-ERROR
000050         GO TO READ-GLBTCH-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070         MOVE 41           TO WS-F-ERROR
000080         PERFORM READ-ERROR.
             GO TO READ-GLBTCH-EXIT.

000150 READ-GLBTCH-EXIT.
000160       EXIT.
