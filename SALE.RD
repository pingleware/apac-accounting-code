
009060 READ-SALE.
009070       READ SALE.
009080     IF WS-STATUS = "00"
009090         GO TO READ-SALE-EXIT.
009100     IF WS-STAT1 = "2" OR "3" OR "4"
009110         MOVE 19           TO WS-F-ERROR
009120         PERFORM READ-ERROR.
009130     IF RECORD-LOCKED
009140         PERFORM LOCKED-RECORD
009150         GO TO READ-SALE.
009160       MOVE 19             TO WS-F-ERROR.
009170       PERFORM READ-ERROR.

009180 READ-SALE-EXIT.
009190       EXIT.
