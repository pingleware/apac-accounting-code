      *
      *    *     ***** ****  ***** ****  *****    ***** *** *     *****
      *    *     *     *   *   *   *   * *        *      *  *     *
      *    *     *     *   *   *   *   * *        *      *  *     *
      *    *     ***   *   *   *   ****  ***      ***    *  *     ***
      *    *     *     *   *   *   *   * *        *      *  *     *
      *    *     *     *   *   *   *   * *        *      *  *     *
      *    ***** ***** ****    *   *   * *        *     *** ***** *****
      *
000020 READ-LEDTRF.
000030       READ LEDTRF
               KEY IS XFR-KEY.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050         GO TO READ-LEDTRF-EXIT.
000040     IF WS-STATUS = "23"
               MOVE 40           TO WS-F-ERROR
000050         GO TO READ-LEDTRF-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070         MOVE 40           TO WS-F-ERROR
000080         PERFORM READ-ERROR.
000090     IF RECORD-LOCKED
	       MOVE W02-LEDTRF	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000100         PERFORM LOCKED-RECORD
000110         GO TO READ-LEDTRF.

000150 READ-LEDTRF-EXIT.
000160       EXIT.
