      *
      *     ***  *****	***   ***  *   *     ****  ****   ***
      *    *   *   *   *   * *	 * *  *      *	 * *   * *   *
      *    *	   *   *   * *	   * *	     *	 * *  *  *
      *     ***    *   *   * *	   **	     ****  ***	 *
      *        *   *   *   * *	   * *	     *	   *  *  *
      *    *   *   *   *   * *	 * *  *      *	   *   * *   *
      *     ***    *	***   ***  *   *     *	   *   *  ***
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
