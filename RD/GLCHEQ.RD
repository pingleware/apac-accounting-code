      *
      *     ***  *     ****  *****  ***  *   *    ***** *** *     *****
      *    *   * *     *   *   *   *   * *   *    *      *  *     *
      *    *     *     *   *   *   *     *   *    *      *  *     *
      *    *     *     ****    *   *     *****    ***    *  *     ***
      *    *  ** *     *   *   *   *     *   *    *      *  *     *
      *    *   * *     *   *   *   *   * *   *    *      *  *     *
      *     ***  ***** ****    *    ***  *   *    *     *** ***** *****
      *
      *==================================================================
      *
      *    The routine READ-ERROR will be performed if a status other than
      *    zero is returned after any READ or START request and requires
      *    that the file number to be moved to WS-F-ERROR before being
      *    performed.
      *
      *    On returning from READ-ERROR, WS-ERROR will contain a value as
      *    follows:
      * 	    0 - No error or status to report
      * 	    1 - Record locked
      * 	    2 - Duplicate key
      * 	    3 - No Record / End of file / Invalid key
      * 	    4 - File already open
      * 	    5 - File already closed
      * 	    6 - Index corrupted
      * 	    7 - Network / Communication error
      * 	    8 - Limit exceeded - Record locks
      * 	    9 - File limit exceeded (files open)
      * 	   10 - Open for output
      * 	   11 - Open for input
      * 	   12 - File locked
      * 	   13 - File not open
      * 	   14 - File not open for I/O
      * 	   99 - All other errors
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
