      *
      *     ***    *   ****  ****  ***** *   *     ***** *** *     *****
      *    *   *  * *  *   * *   * *     *   *     *      *  *     *
      *    *     *   * *   * *   * *      * *      *      *  *     *
      *    *     ***** ****  *   * ***     *       ***    *  *     ***
      *    *     *   * *   * *   * *      * *      *      *  *     *
      *    *   * *   * *   * *   * *     *   *     *      *  *     *
      *     ***  *   * *   * ****  ***** *   *     *     *** ***** *****
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
006080 READ-CARDEX.
006090	     READ CARDEX	 WITH IGNORE LOCK
               KEY IS CRD-KEY.
006100     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110         GO TO READ-CARDEX-EXIT.
	     MOVE 43		 TO WS-F-ERROR.
	     PERFORM READ-ERROR.
	   IF WS-ERROR = 3
	       GO TO READ-CARDEX-RESTORE.
006200	     GO TO READ-CARDEX-END.

006210 READ-CARDEX-LOCK.
006220       READ CARDEX WITH KEPT LOCK
               KEY IS CRD-KEY.
006230     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006240         GO TO READ-CARDEX-EXIT.
	     MOVE 43		 TO WS-F-ERROR.
	     PERFORM READ-ERROR.
	   IF WS-ERROR = 3
	       GO TO READ-CARDEX-RESTORE.
006280	   IF WS-ERROR = 1
006290         PERFORM LOCKED-RECORD
006300         GO TO READ-CARDEX-LOCK.
006310	   IF WS-ERROR = 8
006320         UNLOCK CARDEX
006330         GO TO READ-CARDEX-LOCK.
006360	     GO TO READ-CARDEX-END.

006080 READ-CARDEX-NEXT.
006090       READ CARDEX NEXT.
006100     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110         GO TO READ-CARDEX-EXIT.
	     MOVE 43		 TO WS-F-ERROR.
	     PERFORM READ-ERROR.
	   IF WS-ERROR = 3
	       GO TO READ-CARDEX-RESTORE.
006150	   IF WS-ERROR = 1
006160         PERFORM LOCKED-RECORD
006170         GO TO READ-CARDEX-NEXT.
006200	     GO TO READ-CARDEX-END.

006080 READ-CARDEX-NEXT-LOCK.
006090       READ CARDEX NEXT WITH KEPT LOCK.
006100     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110         GO TO READ-CARDEX-EXIT.
	     MOVE 43		 TO WS-F-ERROR.
	     PERFORM READ-ERROR.
	   IF WS-ERROR = 3
	       GO TO READ-CARDEX-RESTORE.
006150	   IF WS-ERROR = 1
006160         PERFORM LOCKED-RECORD
006170         GO TO READ-CARDEX-NEXT-LOCK.
	   IF WS-ERROR = 8
               UNLOCK CARDEX
               GO TO READ-CARDEX-NEXT-LOCK.
006200	     GO TO READ-CARDEX-END.

006080 READ-CARDEX-PREV.
006090       READ CARDEX PREVIOUS.
006100     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110         GO TO READ-CARDEX-EXIT.
	     PERFORM READ-ERROR.
      *
      *    ****   E N D   O F	F I L E   /   I N V A L I D   K E Y
      *
	   IF WS-ERROR = 3
               MOVE 43           TO WS-F-ERROR
	       GO TO READ-CARDEX-RESTORE.
006150	   IF WS-ERROR = 1
006160         PERFORM LOCKED-RECORD
006170         GO TO READ-CARDEX-PREV.
006200	     GO TO READ-CARDEX-END.

006080 START-AT-CRDX-KEY.
006090       START CARDEX
               KEY >= CRD-KEY.
006100     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110         GO TO READ-CARDEX-EXIT.
	     MOVE 43		 TO WS-F-ERROR.
	     PERFORM READ-ERROR.
	   IF WS-ERROR = 3
	       GO TO READ-CARDEX-RESTORE.
006200	     GO TO READ-CARDEX-END.

006080 START-AT-CRDX-STOCK.
006090       START CARDEX
	       KEY >= CRD-SKEY.
006100     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110         GO TO READ-CARDEX-EXIT.
	     MOVE 43		 TO WS-F-ERROR.
	     PERFORM READ-ERROR.
	   IF WS-ERROR = 3
	       GO TO READ-CARDEX-RESTORE.
006200	     GO TO READ-CARDEX-END.

006080 START-AT-CRDX-MOVE.
006090       START CARDEX
	       KEY >= CRD-MKEY.
006100     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110         GO TO READ-CARDEX-EXIT.
	     MOVE 43		 TO WS-F-ERROR.
	     PERFORM READ-ERROR.
	   IF WS-ERROR = 3
	       GO TO READ-CARDEX-RESTORE.
006200	     GO TO READ-CARDEX-END.

006080 START-AT-CRDX-ACNT.
006090       START CARDEX
	       KEY >= CRD-AKEY.
006100     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110         GO TO READ-CARDEX-EXIT.
	     MOVE 43		 TO WS-F-ERROR.
	     PERFORM READ-ERROR.
	   IF WS-ERROR = 3
	       GO TO READ-CARDEX-RESTORE.
006200	     GO TO READ-CARDEX-END.

006080 START-AT-CRDX-DATE.
006090       START CARDEX
	       KEY >= CRD-DKEY.
006100     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110         GO TO READ-CARDEX-EXIT.
	     MOVE 43		 TO WS-F-ERROR.
	     PERFORM READ-ERROR.
	   IF WS-ERROR = 3
	       GO TO READ-CARDEX-RESTORE.
006200	     GO TO READ-CARDEX-END.

006080 START-AT-CRDX-SDTE.
006090       START CARDEX
	       KEY >= CRD-SDKEY.
006100     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110         GO TO READ-CARDEX-EXIT.
	     MOVE 43		 TO WS-F-ERROR.
	     PERFORM READ-ERROR.
	   IF WS-ERROR = 3
	       GO TO READ-CARDEX-RESTORE.
006200	     GO TO READ-CARDEX-END.

006080 START-AT-CRDX-DSTK.
006090       START CARDEX
	       KEY >= CRD-DSKEY.
006100     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110         GO TO READ-CARDEX-EXIT.
	     MOVE 43		 TO WS-F-ERROR.
	     PERFORM READ-ERROR.
	   IF WS-ERROR = 3
	       GO TO READ-CARDEX-RESTORE.

000470 READ-CARDEX-END.
	   IF NOT (WS-ERROR = ZERO)
	       STOP RUN.

       READ-CARDEX-RESTORE.
	     PERFORM RESTORE-SCREEN.
	     MOVE "N"		 TO WS-SKIP.

006520 READ-CARDEX-EXIT.
006530       EXIT.
