      *    旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
      *    �      S e t   u p   t h e   F U N C T I O N   k e y s      �
      *    읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
             MOVE 1                  TO USER-ACTION USER-SETTING.
      *    旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
      *    �                    ESC, F1 to F10 keys                    �
      *    읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
             MOVE ZERO               TO USER-NUMBER.
             MOVE 11                 TO USER-KEYS.
             CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
      *    �                 PAGE-UP and PAGE-DOWN keys                �
      *    읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
             MOVE 53                 TO USER-NUMBER.
             MOVE 2                  TO USER-KEYS.
             CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
      *    �                 UP and DOWN ARROW keys                    �
      *    읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
             MOVE 2                  TO USER-ACTION.
             MOVE 3                  TO USER-SETTING.
             MOVE 5                  TO USER-NUMBER.
             MOVE 2                  TO USER-KEYS.
             CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
      *    �               A C T I V A T E   M O U S E                 �
      *    읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
             MOVE 64                 TO MOUSE-FUNC.
             MOVE 1                  TO MOUSE-PARAM.
             CALL X"AF" USING MOUSE-FUNC MOUSE-PARAM.
           IF MOUSE-FUNC NOT = 255
               MOVE "Y"              TO MOUSE-ATTACHED
               MOVE 66               TO MOUSE-FUNC
               MOVE 0                TO MOUSE-PARAM
               CALL X"AF" USING MOUSE-FUNC MOUSE-PARAM
           ELSE
               GO TO ZA00A.
      *    旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
      *    �            Set MOUSE key to act as FUNCTION key           �
      *    읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
             MOVE 3                  TO USER-ACTION.
             MOVE 27                 TO USER-NUMBER.
             MOVE 2                  TO USER-KEYS.
             CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
