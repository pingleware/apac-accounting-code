             EVALUATE WS-STATUS
               WHEN "10"    MOVE "End of FILE"             TO WS-STAT-MESSAGE
               WHEN "22"    MOVE "Duplicate record number" TO WS-STAT-MESSAGE
               WHEN "23"    MOVE "Invalid record number"   TO WS-STAT-MESSAGE
               WHEN "24"    MOVE "DISK full"               TO WS-STAT-MESSAGE
               WHEN "30"    MOVE "DISK error"              TO WS-STAT-MESSAGE
               WHEN "94"    MOVE "FILE locked"             TO WS-STAT-MESSAGE
             END-EVALUATE.
             PERFORM HEADING-AND-CLEAR-SCREEN.      
             DISPLAY WS-IO-ERROR                                   AT 1212           WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             DISPLAY "File - "                                     AT 1405 WS-FILE   WITH FOREGROUND-COLOR Cyan  HIGHLIGHT.
             DISPLAY "Status "                                     AT 1605 WS-STATUS WITH FOREGROUND-COLOR Cyan  HIGHLIGHT ": " WS-STAT-MESSAGE WITH FOREGROUND-COLOR Brown HIGHLIGHT.
           IF WS-STATUS NOT = "94"
               DISPLAY "Key    "                                   AT 1805 WS-KEY    WITH FOREGROUND-COLOR Cyan  HIGHLIGHT " " WS-KEYX WITH FOREGROUND-COLOR Cyan HIGHLIGHT
             DISPLAY "Reverse backup or contact program Support" AT 2005.
             DISPLAY "Please make a note of these details" AT 2205.
             STOP RUN.
