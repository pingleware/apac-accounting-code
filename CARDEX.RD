      *
      *     ***    *   ****  ****  ***** *   *     ***** *** *     *****
      *    *   *  * *  *   * *   * *     *   *     *      *  *     *
      *    *     *   * *   * *   * *      * *      *      *  *     *
      *    *     ***** ****  *   * ***     *       ***    *  *     ***
      *    *     *   * *   * *   * *      * *      *      *  *     *
      *    *   * *   * *   * *   * *     *   *     *      *  *     *
      *     ***  *   * *   * ****  ***** *   *     *     *** ***** *****
      *
006080 READ-CARDEX.
006090	     READ CARDEX	 WITH IGNORE LOCK
               KEY IS CRD-KEY.
006100     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110         GO TO READ-CARDEX-EXIT.
           IF WS-STATUS = "23"
               MOVE 43           TO WS-F-ERROR
               GO TO READ-CARDEX-EXIT.
006120     IF WS-STAT1 = "2" OR "3" OR "4"
006130         MOVE 43           TO WS-F-ERROR
006140         PERFORM READ-ERROR.
006150*     IF RECORD-LOCKED
006160* 	PERFORM LOCKED-RECORD
006170* 	GO TO READ-CARDEX.
006200       GO TO READ-CARDEX-EXIT.

006210 READ-CARDEX-LOCK.
006220       READ CARDEX WITH KEPT LOCK
               KEY IS CRD-KEY.
006230     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006240         GO TO READ-CARDEX-EXIT.
           IF WS-STATUS = "23"
               MOVE 43           TO WS-F-ERROR
               GO TO READ-CARDEX-EXIT.
006250     IF WS-STAT1 = "2" OR "3" OR "4"
006260         MOVE 43           TO WS-F-ERROR
006270         PERFORM READ-ERROR.
006280     IF RECORD-LOCKED
006290         PERFORM LOCKED-RECORD
006300         GO TO READ-CARDEX-LOCK.
006310     IF TOO-MANY-LOCKS
006320         UNLOCK CARDEX
006330         GO TO READ-CARDEX-LOCK.
006360       GO TO READ-CARDEX-EXIT.

006080 READ-CARDEX-NEXT.
006090       READ CARDEX NEXT.
006100     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110         GO TO READ-CARDEX-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
               MOVE 43           TO WS-F-ERROR
               GO TO READ-CARDEX-EXIT.
006120     IF WS-STAT1 = "2" OR "3" OR "4"
006130         MOVE 43           TO WS-F-ERROR
006140         PERFORM READ-ERROR.
006150     IF RECORD-LOCKED
006160         PERFORM LOCKED-RECORD
006170         GO TO READ-CARDEX-NEXT.
006200       GO TO READ-CARDEX-EXIT.

006080 READ-CARDEX-NEXT-LOCK.
006090       READ CARDEX NEXT WITH KEPT LOCK.
006100     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110         GO TO READ-CARDEX-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
               MOVE 43           TO WS-F-ERROR
               GO TO READ-CARDEX-EXIT.
006120     IF WS-STAT1 = "2" OR "3" OR "4"
006130         MOVE 43           TO WS-F-ERROR
006140         PERFORM READ-ERROR.
006150     IF RECORD-LOCKED
006160         PERFORM LOCKED-RECORD
006170         GO TO READ-CARDEX-NEXT-LOCK.
           IF TOO-MANY-LOCKS
               UNLOCK CARDEX
               GO TO READ-CARDEX-NEXT-LOCK.
006200       GO TO READ-CARDEX-EXIT.

006080 READ-CARDEX-PREV.
006090       READ CARDEX PREVIOUS.
006100     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110         GO TO READ-CARDEX-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
               MOVE 43           TO WS-F-ERROR
               GO TO READ-CARDEX-EXIT.
006120     IF WS-STAT1 = "2" OR "3" OR "4"
006130         MOVE 43           TO WS-F-ERROR
006140         PERFORM READ-ERROR.
006150     IF RECORD-LOCKED
006160         PERFORM LOCKED-RECORD
006170         GO TO READ-CARDEX-PREV.
006200       GO TO READ-CARDEX-EXIT.

006080 START-AT-CRDX-KEY.
006090       START CARDEX
               KEY >= CRD-KEY.
006100     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110         GO TO READ-CARDEX-EXIT.
           IF WS-STATUS = "23"
               MOVE 43           TO WS-F-ERROR
               GO TO READ-CARDEX-EXIT.
006120     IF WS-STAT1 = "2" OR "3" OR "4"
006130         MOVE 43           TO WS-F-ERROR
006140         PERFORM READ-ERROR.
006200       GO TO READ-CARDEX-EXIT.

006080 START-AT-CRDX-STOCK.
006090       START CARDEX
	       KEY >= CRD-SKEY.
006100     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110         GO TO READ-CARDEX-EXIT.
           IF WS-STATUS = "23"
               MOVE 43           TO WS-F-ERROR
               GO TO READ-CARDEX-EXIT.
006120     IF WS-STAT1 = "2" OR "3" OR "4"
006130         MOVE 43           TO WS-F-ERROR
006140         PERFORM READ-ERROR.
006200       GO TO READ-CARDEX-EXIT.

006080 START-AT-CRDX-MOVE.
006090       START CARDEX
	       KEY >= CRD-MKEY.
006100     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110         GO TO READ-CARDEX-EXIT.
           IF WS-STATUS = "23"
               MOVE 43           TO WS-F-ERROR
               GO TO READ-CARDEX-EXIT.
006120     IF WS-STAT1 = "2" OR "3" OR "4"
006130         MOVE 43           TO WS-F-ERROR
006140         PERFORM READ-ERROR.
006200	     GO TO READ-CARDEX-EXIT.

006080 START-AT-CRDX-ACNT.
006090       START CARDEX
	       KEY >= CRD-AKEY.
006100     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110         GO TO READ-CARDEX-EXIT.
           IF WS-STATUS = "23"
               MOVE 43           TO WS-F-ERROR
               GO TO READ-CARDEX-EXIT.
006120     IF WS-STAT1 = "2" OR "3" OR "4"
006130         MOVE 43           TO WS-F-ERROR
006140         PERFORM READ-ERROR.
006200	     GO TO READ-CARDEX-EXIT.

006080 START-AT-CRDX-DATE.
006090       START CARDEX
	       KEY >= CRD-DKEY.
006100     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110         GO TO READ-CARDEX-EXIT.
           IF WS-STATUS = "23"
               MOVE 43           TO WS-F-ERROR
               GO TO READ-CARDEX-EXIT.
006120     IF WS-STAT1 = "2" OR "3" OR "4"
006130         MOVE 43           TO WS-F-ERROR
006140         PERFORM READ-ERROR.
006200	     GO TO READ-CARDEX-EXIT.

006080 START-AT-CRDX-SDTE.
006090       START CARDEX
	       KEY >= CRD-SDKEY.
006100     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110         GO TO READ-CARDEX-EXIT.
           IF WS-STATUS = "23"
               MOVE 43           TO WS-F-ERROR
               GO TO READ-CARDEX-EXIT.
006120     IF WS-STAT1 = "2" OR "3" OR "4"
006130         MOVE 43           TO WS-F-ERROR
006140         PERFORM READ-ERROR.
006200	     GO TO READ-CARDEX-EXIT.

006080 START-AT-CRDX-DSTK.
006090       START CARDEX
	       KEY >= CRD-DSKEY.
006100     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110         GO TO READ-CARDEX-EXIT.
           IF WS-STATUS = "23"
               MOVE 43           TO WS-F-ERROR
               GO TO READ-CARDEX-EXIT.
006120     IF WS-STAT1 = "2" OR "3" OR "4"
006130         MOVE 43           TO WS-F-ERROR
006140         PERFORM READ-ERROR.


006520 READ-CARDEX-EXIT.
006530       EXIT.
