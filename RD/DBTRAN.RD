      *
      *    ****  ****  ***** ****    *   *   *    ***** *** *     *****
      *    *   * *   *   *   *   *  * *  *   *    *      *  *     *
      *    *   * *   *   *   *   * *   * **  *    *      *  *     *
      *    *   * ****    *   ****  ***** * * *    ***    *  *     ***
      *    *   * *   *   *   *   * *   * *  **    *      *  *     *
      *    *   * *   *   *   *   * *   * *   *    *      *  *     *
      *    ****  ****    *   *   * *   * *   *    *     *** ***** *****
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
000020 READ-DBTRAN.
000030       READ DBTRAN
               KEY IS TRA-KEY.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050         GO TO READ-DBTRAN-EXIT.
           IF WS-STATUS = "23"
               MOVE 5            TO WS-F-ERROR
000050         GO TO READ-DBTRAN-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070         MOVE 5            TO WS-F-ERROR
000080         PERFORM READ-ERROR.
000090     IF RECORD-LOCKED
	       MOVE W02-DBTRAN	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000100         PERFORM LOCKED-RECORD
000110         GO TO READ-DBTRAN.
000050       GO TO READ-DBTRAN-EXIT.

000020 READ-DBTRAN-NEXT.
000030       READ DBTRAN NEXT.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050         GO TO READ-DBTRAN-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
               MOVE 5            TO WS-F-ERROR
000050         GO TO READ-DBTRAN-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070         MOVE 5            TO WS-F-ERROR
000080         PERFORM READ-ERROR.
000090     IF RECORD-LOCKED
	       MOVE W02-DBTRAN	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000100         PERFORM LOCKED-RECORD
000110         GO TO READ-DBTRAN-NEXT.
000050       GO TO READ-DBTRAN-EXIT.

000020 READ-DBTRAN-PREV.
000030       READ DBTRAN PREVIOUS.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050         GO TO READ-DBTRAN-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
               MOVE 5            TO WS-F-ERROR
000050         GO TO READ-DBTRAN-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070         MOVE 5            TO WS-F-ERROR
000080         PERFORM READ-ERROR.
000090     IF RECORD-LOCKED
	       MOVE W02-DBTRAN	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000100         PERFORM LOCKED-RECORD
000110         GO TO READ-DBTRAN-PREV.
000050       GO TO READ-DBTRAN-EXIT.

000020 START-AT-DTRN-KEY.
000030       START DBTRAN
               KEY >= TRA-KEY.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050         GO TO READ-DBTRAN-EXIT.
           IF WS-STATUS = "23"
               MOVE 5            TO WS-F-ERROR
000050         GO TO READ-DBTRAN-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070         MOVE 5            TO WS-F-ERROR
000080         PERFORM READ-ERROR.
             GO TO READ-DBTRAN-EXIT.

000020 START-AT-DTRN-CODE.
000030       START DBTRAN
               KEY >= TRA-TRSEQ.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050         GO TO READ-DBTRAN-EXIT.
           IF WS-STATUS = "23"
               MOVE 5            TO WS-F-ERROR
000050         GO TO READ-DBTRAN-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070         MOVE 5            TO WS-F-ERROR
000080         PERFORM READ-ERROR.
             GO TO READ-DBTRAN-EXIT.

000150 READ-DBTRAN-EXIT.
000160       EXIT.
