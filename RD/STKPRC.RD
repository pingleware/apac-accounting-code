      *
      *     ***  *****	***   ***  *   *     ****  ****   ***
      *    *   *   *   *   * *	 * *  *      *	 * *   * *   *
      *    *	   *   *   * *	   * *	     *	 * *  *  *
      *     ***    *   *   * *	   **	     ****  ***	 *
      *        *   *   *   * *	   * *	     *	   *  *  *
      *    *   *   *   *   * *	 * *  *      *	   *   * *   *
      *     ***    *	***   ***  *   *     *	   *   *  ***
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
000010 READ-STKPRC.
000020	     READ STKPRC.
000040     IF WS-STATUS = "00"
000050         MOVE ZERO         TO WS-F-ERROR
000060	       GO TO READ-STKPRC-EXIT.
000070     IF WS-STATUS = "23"
000080	       MOVE 34		 TO WS-F-ERROR
000090	       GO TO READ-STKPRC-EXIT.
000100     IF WS-STAT1 = "2" OR "3" OR "4"
000110	       MOVE 34		 TO WS-F-ERROR
000120         PERFORM READ-ERROR.
000130     IF RECORD-LOCKED
000140         PERFORM LOCKED-RECORD
000150	       GO TO READ-STKPRC.

001580 READ-STKPRC-EXIT.
001590       EXIT.
