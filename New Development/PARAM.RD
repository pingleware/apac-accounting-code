      *
      *    ****    *   ****    *    *     *     ***** *** *     *****
      *    *   *  * *  *   *  * *   **   **     *      *  *     *
      *    *   * *   * *   * *   *  * * * *     *      *  *     *
      *    ****  ***** ****  *****  *  *  *     ***    *  *     ***
      *    *     *   * *  *  *   *  *     *     *      *  *     *
      *    *     *   * *   * *   *  *     *     *      *  *     *
      *    *     *   * *   * *   *  *     *     *     *** ***** *****
      *
       READ-PARAM.
             READ PARAM IGNORE LOCK.
           IF WS-STATUS = "00"
               MOVE 1                TO WS-COUNT
               GO TO READ-PARAM-EXIT.
             MOVE 15                 TO WS-F-ERROR.
             PERFORM READ-ERROR.
      
       READ-PARAM-LOCK.
             READ PARAM WITH KEPT LOCK.
           IF WS-STATUS = "00"
               MOVE 1                TO WS-COUNT
               GO TO READ-PARAM-EXIT.
           IF WS-STAT1 = "2" OR "3" OR "4"
               MOVE 15               TO WS-F-ERROR
               PERFORM READ-ERROR.
           IF RECORD-LOCKED
               IF WS-PARKEY = "4"
                   UNLOCK PARAM
                   PERFORM LOCK-USERS-REC THRU LOCK-REC-EXIT WS-COUNT TIMES
                   ADD 1             TO WS-COUNT
                   IF WS-COUNT > 25
                       MOVE 1        TO WS-COUNT
                   END-IF
                   GO TO READ-PARAM-LOCK
               ELSE
                   MOVE W02-PARAM    TO WS-FILE
                   MOVE WS-PARKEY    TO WS-KEY
                   PERFORM LOCKED-RECORD WS-COUNT TIMES
                   ADD 1             TO WS-COUNT
                   IF WS-COUNT > 20
                       MOVE 1        TO WS-COUNT
                   END-IF
                   GO TO READ-PARAM-LOCK.
           IF TOO-MANY-LOCKS
               UNLOCK PARAM
               GO TO READ-PARAM-LOCK.
             MOVE 15                 TO WS-F-ERROR.
             PERFORM READ-ERROR.
      
       READ-PARAM-LOCK-END.
           READ PARAM WITH KEPT LOCK.
           IF WS-STATUS = "00"
               GO TO READ-PARAM-EXIT.
           IF RECORD-LOCKED
               DISPLAY SPACE AT 5050
               DISPLAY "PLEASE BE PATIENT" AT 5051 WITH FOREGROUND-COLOR Brown HIGHLIGHT BLINK
               MOVE ZERO             TO WS-KEY
               PERFORM ONE-SECOND-DELAY THRU LOCK-REC-EXIT
               GO TO READ-PARAM-LOCK-END.
           IF TOO-MANY-LOCKS UNLOCK PARAM
               GO TO READ-PARAM-LOCK-END.
             MOVE 15                 TO WS-F-ERROR.
             PERFORM READ-ERROR.
      
       READ-PARAM-EXIT.
             EXIT.
