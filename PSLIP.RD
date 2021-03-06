
000020 READ-PSLIP.
000030	     READ PSLIP
	       KEY IS PIC-KEY.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-PSLIP-EXIT.
           IF WS-STATUS = "23"
	       MOVE 34		 TO WS-F-ERROR
000050	       GO TO READ-PSLIP-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 34		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
000090     IF RECORD-LOCKED
	       MOVE W02-PSLIP	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000100         PERFORM LOCKED-RECORD
000110	       GO TO READ-PSLIP.
000050	     GO TO READ-PSLIP-EXIT.

000020 READ-PSLIP-NEXT.
000030	     READ PSLIP NEXT.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-PSLIP-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
	       MOVE 34		 TO WS-F-ERROR
000050	       GO TO READ-PSLIP-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 34		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
000090     IF RECORD-LOCKED
	       MOVE W02-PSLIP	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000100         PERFORM LOCKED-RECORD
000110	       GO TO READ-PSLIP-NEXT.
000050	     GO TO READ-PSLIP-EXIT.

000020 READ-PSLIP-PREV.
000030	     READ PSLIP PREVIOUS.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-PSLIP-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
	       MOVE 34		 TO WS-F-ERROR
000050	       GO TO READ-PSLIP-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 34		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
000090     IF RECORD-LOCKED
	       MOVE W02-PSLIP	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000100         PERFORM LOCKED-RECORD
000110	       GO TO READ-PSLIP-PREV.
000050	     GO TO READ-PSLIP-EXIT.

000020 START-AT-PSLIP-KEY.
000030	     START PSLIP
	       KEY >= PIC-KEY.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-PSLIP-EXIT.
           IF WS-STATUS = "23"
	       MOVE 34		 TO WS-F-ERROR
000050	       GO TO READ-PSLIP-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 34		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
	     GO TO READ-PSLIP-EXIT.

000020 START-AT-PSLIP-TYPE.
000030	     START PSLIP
	       KEY >= PIC-TKEY.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-PSLIP-EXIT.
           IF WS-STATUS = "23"
	       MOVE 34		 TO WS-F-ERROR
000050	       GO TO READ-PSLIP-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 34		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
	     GO TO READ-PSLIP-EXIT.

000020 START-AT-PSLIP-DRAW.
000030	     START PSLIP
	       KEY >= PIC-DKEY.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-PSLIP-EXIT.
           IF WS-STATUS = "23"
	       MOVE 34		 TO WS-F-ERROR
000050	       GO TO READ-PSLIP-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 34		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
	     GO TO READ-PSLIP-EXIT.

000150 READ-PSLIP-EXIT.
000160       EXIT.
