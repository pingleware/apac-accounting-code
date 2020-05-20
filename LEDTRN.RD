      *
      *    *     ***** ****  ***** ****  *   *    ***** *** *     *****
      *    *     *     *   *   *   *   * *   *    *      *  *     *
      *    *     *     *   *   *   *   * **  *    *      *  *     *
      *    *     ***   *   *   *   ****  * * *    ***    *  *     ***
      *    *     *     *   *   *   *   * *  **    *      *  *     *
      *    *     *     *   *   *   *   * *   *    *      *  *     *
      *    ***** ***** ****    *   *   * *   *    *     *** ***** *****
      *
000020 READ-LEDTRN.
000030	     READ LEDTRN	 WITH IGNORE LOCK.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050         GO TO READ-LEDTRN-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070         MOVE 39           TO WS-F-ERROR
000080         PERFORM READ-ERROR.

000150 READ-LEDTRN-EXIT.
000160       EXIT.
