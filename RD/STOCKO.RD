      *
      *     ***  *****  ***   ***  *   *     ***** ***** *     *****
      *    *   *   *   *   * *   * *  *      *       *   *     *
      *    *       *   *   * *     * *       *       *   *     *
      *     ***    *   *   * *     **        ***     *   *     ***
      *        *   *   *   * *     * *       *       *   *     *
      *    *   *   *   *   * *   * *  *      *       *   *     *
      *     ***    *    ***   ***  *   *     *     ***** ***** *****
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
000010 READ-STOCK.
000020	     READ STOCK 	 WITH IGNORE LOCK
000030         KEY IS STK-CODE.
000040	   IF WS-STATUS = "00"
	       IF STK-DISC NOT = 9
000050		   MOVE ZERO	 TO WS-F-ERROR
000060		   GO TO READ-STOCK-EXIT
	       ELSE
		   MOVE 22	 TO WS-F-ERROR
000060		   GO TO READ-STOCK-EXIT.
000070	   IF WS-STATUS = "23"
000080         MOVE 22           TO WS-F-ERROR
000090         GO TO READ-STOCK-EXIT.
000100     IF WS-STAT1 = "2" OR "3" OR "4"
000110         MOVE 22           TO WS-F-ERROR
000120         PERFORM READ-ERROR.
000160       GO TO READ-STOCK-EXIT.

000180 READ-STOCK-LOCK.
000190	     READ STOCK 	 WITH KEPT LOCK
000200         KEY IS STK-CODE.
000210     IF WS-STATUS = "00"
	       IF STK-DISC NOT = 9
000050		   MOVE ZERO	 TO WS-F-ERROR
000060		   GO TO READ-STOCK-EXIT
	       ELSE
		   MOVE 22	 TO WS-F-ERROR
000060		   GO TO READ-STOCK-EXIT.
000240	   IF WS-STATUS = "23"
000250         MOVE 22           TO WS-F-ERROR
000260         GO TO READ-STOCK-EXIT.
000270     IF WS-STAT1 = "2" OR "3" OR "4"
000280         MOVE 22           TO WS-F-ERROR
000290         PERFORM READ-ERROR.
000300     IF RECORD-LOCKED
	       MOVE W02-STOCKF	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000310         PERFORM LOCKED-RECORD
000320         GO TO READ-STOCK-LOCK.
000330     IF TOO-MANY-LOCKS
000340         UNLOCK STOCK
000350         GO TO READ-STOCK-LOCK.
000360       GO TO READ-STOCK-EXIT.

000380 READ-STOCK-NEXT.
000390	     READ STOCK NEXT	 WITH IGNORE LOCK.
000400     IF WS-STATUS = "00"
	       IF STK-DISC NOT = 9
000050		   MOVE ZERO	 TO WS-F-ERROR
000060		   GO TO READ-STOCK-EXIT
	       ELSE
000060		   GO TO READ-STOCK-NEXT.
000430	   IF (WS-STATUS = "23") OR
000440        (WS-STAT1 = "1")
000450         MOVE 22           TO WS-F-ERROR
000460         GO TO READ-STOCK-EXIT.
000470     IF WS-STAT1 = "2" OR "3" OR "4"
000480         MOVE 22           TO WS-F-ERROR
000490         PERFORM READ-ERROR.
000530       GO TO READ-STOCK-EXIT.

000550 READ-STOCK-NEXT-LOCK.
000560       READ STOCK NEXT WITH KEPT LOCK.
000570     IF WS-STATUS = "00"
	       IF STK-DISC NOT = 9
000050		   MOVE ZERO	 TO WS-F-ERROR
000060		   GO TO READ-STOCK-EXIT
	       ELSE
000060		   GO TO READ-STOCK-NEXT-LOCK.
000600	   IF (WS-STATUS = "23") OR
000610        (WS-STAT1 = "1")
000620         MOVE 22           TO WS-F-ERROR
000630         GO TO READ-STOCK-EXIT.
000640     IF WS-STAT1 = "2" OR "3" OR "4"
000650         MOVE 22           TO WS-F-ERROR
000660         PERFORM READ-ERROR.
000670     IF RECORD-LOCKED
	       MOVE W02-STOCKF	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000680         PERFORM LOCKED-RECORD
000690         GO TO READ-STOCK-NEXT-LOCK.
000700     IF TOO-MANY-LOCKS
000710         UNLOCK STOCK
000720         GO TO READ-STOCK-NEXT-LOCK.
000730       GO TO READ-STOCK-EXIT.

000380 READ-STOCK-PREV.
000390	     READ STOCK PREVIOUS WITH IGNORE LOCK.
000400     IF WS-STATUS = "00"
	       IF STK-DISC NOT = 9
000050		   MOVE ZERO	 TO WS-F-ERROR
000060		   GO TO READ-STOCK-EXIT
	       ELSE
000060		   GO TO READ-STOCK-PREV.
000430	   IF (WS-STATUS = "23") OR
000440        (WS-STAT1 = "1")
000450         MOVE 22           TO WS-F-ERROR
000460         GO TO READ-STOCK-EXIT.
000470     IF WS-STAT1 = "2" OR "3" OR "4"
000480         MOVE 22           TO WS-F-ERROR
000490         PERFORM READ-ERROR.
000530       GO TO READ-STOCK-EXIT.

000750 START-AT-STOCK-CODE.
000760       START STOCK 
000770         KEY >= STK-CODE.
000780     IF WS-STATUS = "00"
000790         MOVE ZERO         TO WS-F-ERROR
000800         GO TO READ-STOCK-EXIT.
000810     IF WS-STATUS = "23"
000820         MOVE 22           TO WS-F-ERROR
000830         GO TO READ-STOCK-EXIT.
000840     IF WS-STAT1 = "2" OR "3" OR "4"
000850         MOVE 22           TO WS-F-ERROR
000860         PERFORM READ-ERROR.
000870       GO TO READ-STOCK-EXIT.

000890 START-AT-ALT-CODE.
000900       START STOCK
000910         KEY >= STK-ACODE.
000920     IF WS-STATUS = "00"
000930         MOVE ZERO         TO WS-F-ERROR
000940         GO TO READ-STOCK-EXIT.
000950     IF WS-STATUS = "23"
000960         MOVE 22           TO WS-F-ERROR
000970         GO TO READ-STOCK-EXIT.
000980     IF WS-STAT1 = "2" OR "3" OR "4"
000990         MOVE 22           TO WS-F-ERROR
001000         PERFORM READ-ERROR.
001010       GO TO READ-STOCK-EXIT.

001030 START-AT-STOCK-BIN.
001040       START STOCK
001050         KEY >= STK-BSEQ.
001060     IF WS-STATUS = "00"
001070         MOVE ZERO         TO WS-F-ERROR
001080         GO TO READ-STOCK-EXIT.
001090     IF WS-STATUS = "23"
001100         MOVE 22           TO WS-F-ERROR
001110         GO TO READ-STOCK-EXIT.
001120     IF WS-STAT1 = "2" OR "3" OR "4"
001130         MOVE 22           TO WS-F-ERROR
001140         PERFORM READ-ERROR.
001150       GO TO READ-STOCK-EXIT.

001170 START-AT-STOCK-LEDG.
001180       START STOCK
001190         KEY >= STK-LSEQ.
001200     IF WS-STATUS = "00"
001210         MOVE ZERO         TO WS-F-ERROR
001220         GO TO READ-STOCK-EXIT.
001230     IF WS-STATUS = "23"
001240         MOVE 22           TO WS-F-ERROR
001250         GO TO READ-STOCK-EXIT.
001260     IF WS-STAT1 = "2" OR "3" OR "4"
001270         MOVE 22           TO WS-F-ERROR
001280         PERFORM READ-ERROR.
001290       GO TO READ-STOCK-EXIT.

001310 START-AT-STOCK-DESC.
001320       START STOCK
001330         KEY >= STK-DKEY.
001340     IF WS-STATUS = "00"
001350         MOVE ZERO         TO WS-F-ERROR
001360         GO TO READ-STOCK-EXIT.
001370     IF WS-STATUS = "23"
001380         MOVE 22           TO WS-F-ERROR
001390         GO TO READ-STOCK-EXIT.
001400     IF WS-STAT1 = "2" OR "3" OR "4"
001410         MOVE 22           TO WS-F-ERROR
001420         PERFORM READ-ERROR.
001430       GO TO READ-STOCK-EXIT.

001450 START-AT-STOCK-XREF.
001460       START STOCK
001470         KEY >= STK-XREF.
001480     IF WS-STATUS = "00"
001490         MOVE ZERO         TO WS-F-ERROR
001500         GO TO READ-STOCK-EXIT.
001510     IF WS-STATUS = "23"
001520         MOVE 22           TO WS-F-ERROR
001530         GO TO READ-STOCK-EXIT.
001540     IF WS-STAT1 = "2" OR "3" OR "4"
001550         MOVE 22           TO WS-F-ERROR
001560         PERFORM READ-ERROR.

001580 READ-STOCK-EXIT.
001590       EXIT.
