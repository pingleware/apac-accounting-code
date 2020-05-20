      *
      * *****  ***  ****   ***	*****	*   *****    ***** *** *    ****
      *   *   *   * *	* *   *   *    * *    *      *	    *  *    *
      *   *   *   * *	* *	  *   *   *   *      *	    *  *    *
      *   *   *   * ****   ***	  *   *****   *      ***    *  *    ***
      *   *   *   * *	*     *   *   *   *   *      *	    *  *    *
      * * *   *   * *	* *   *   *   *   *   *      *	    *  *    *
      * '*'    ***  ****   ***	  *   *   *   *      *	   *** **** ****
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
000010 READ-JOBSTAT.
000020	     READ JOBSTAT  WITH IGNORE LOCK
000030	       KEY IS JST-KEY.
000040     IF WS-STATUS = "00"
000050         MOVE ZERO         TO WS-F-ERROR
000060	       GO TO READ-JOBSTAT-EXIT.
000070     IF WS-STATUS = 23
000080	       MOVE 60		 TO WS-F-ERROR
000090	       GO TO READ-JOBSTAT-EXIT.
000100     IF WS-STAT1 = "2" OR "3" OR "4"
000110	       MOVE 60		 TO WS-F-ERROR
000120         PERFORM READ-ERROR.
000160	     GO TO READ-JOBSTAT-EXIT.
000170
000180 READ-JOBSTAT-LOCK.
000190	     READ JOBSTAT  WITH KEPT LOCK
000200	       KEY IS ITM-MODEL-NO.
000210     IF WS-STATUS = "00"
000220         MOVE ZERO         TO WS-F-ERROR
000230	       GO TO READ-JOBSTAT-EXIT.
000240     IF WS-STATUS = 23
000250	       MOVE 60		 TO WS-F-ERROR
000260	       GO TO READ-JOBSTAT-EXIT.
000270     IF WS-STAT1 = "2" OR "3" OR "4"
000280	       MOVE 60		 TO WS-F-ERROR
000290         PERFORM READ-ERROR.
000300     IF RECORD-LOCKED
	       MOVE W02-JOBSTAT	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000310         PERFORM LOCKED-RECORD
000320	       GO TO READ-JOBSTAT-LOCK.
000330     IF TOO-MANY-LOCKS
000340	       UNLOCK JOBSTAT
000350	       GO TO READ-JOBSTAT-LOCK.
000360	     GO TO READ-JOBSTAT-EXIT.

000380 READ-JOBSTAT-NEXT.
000390	     READ JOBSTAT NEXT	 WITH IGNORE LOCK.
000400     IF WS-STATUS = "00"
000410         MOVE ZERO         TO WS-F-ERROR
000420	       GO TO READ-JOBSTAT-EXIT.
000430     IF (WS-STATUS = "23") OR
000440        (WS-STAT1 = "1")
000450	       MOVE 60		 TO WS-F-ERROR
000460	       GO TO READ-JOBSTAT-EXIT.
000470     IF WS-STAT1 = "2" OR "3" OR "4"
000480	       MOVE 60		 TO WS-F-ERROR
000490         PERFORM READ-ERROR.
000530	     GO TO READ-JOBSTAT-EXIT.

000380 READ-JOBSTAT-PREV.
000390	     READ JOBSTAT PREVIOUS WITH IGNORE LOCK.
000400     IF WS-STATUS = "00"
000410         MOVE ZERO         TO WS-F-ERROR
000420	       GO TO READ-JOBSTAT-EXIT.
000430     IF (WS-STATUS = "23") OR
000440        (WS-STAT1 = "1")
000450	       MOVE 60		 TO WS-F-ERROR
000460	       GO TO READ-JOBSTAT-EXIT.
000470     IF WS-STAT1 = "2" OR "3" OR "4"
000480	       MOVE 60		 TO WS-F-ERROR
000490         PERFORM READ-ERROR.
000530	     GO TO READ-JOBSTAT-EXIT.

000550 READ-JOBSTAT-NEXT-LOCK.
000560	     READ JOBSTAT NEXT WITH KEPT LOCK.
000570     IF WS-STATUS = "00"
000580         MOVE ZERO         TO WS-F-ERROR
000590	       GO TO READ-JOBSTAT-EXIT.
000600     IF (WS-STATUS = "23") OR
000610        (WS-STAT1 = "1")
000620	       MOVE 60		 TO WS-F-ERROR
000630	       GO TO READ-JOBSTAT-EXIT.
000640     IF WS-STAT1 = "2" OR "3" OR "4"
000650	       MOVE 60		 TO WS-F-ERROR
000660         PERFORM READ-ERROR.
000670     IF RECORD-LOCKED
	       MOVE W02-JOBSTAT	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000680         PERFORM LOCKED-RECORD
000690	       GO TO READ-JOBSTAT-NEXT-LOCK.
000700     IF TOO-MANY-LOCKS
000710	       UNLOCK JOBSTAT
000720	       GO TO READ-JOBSTAT-NEXT-LOCK.
000730	     GO TO READ-JOBSTAT-EXIT.

000750 START-AT-JOBSTAT-KEY.
000760	     START JOBSTAT
000770	       KEY >= JST-KEY.
000780     IF WS-STATUS = "00"
000790         MOVE ZERO         TO WS-F-ERROR
000800	       GO TO READ-JOBSTAT-EXIT.
000810     IF WS-STATUS = "23"
000820	       MOVE 60		 TO WS-F-ERROR
000830	       GO TO READ-JOBSTAT-EXIT.
000840     IF WS-STAT1 = "2" OR "3" OR "4"
000850	       MOVE 60		 TO WS-F-ERROR
000860         PERFORM READ-ERROR.
000870	     GO TO READ-JOBSTAT-EXIT.

001160 READ-JOBSTAT-EXIT.
001170       EXIT.
