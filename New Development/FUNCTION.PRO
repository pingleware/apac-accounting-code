      *    �����������������������������������������������������������Ŀ
      *    �      S e t   u p   t h e   F U N C T I O N   k e y s      �
      *    �������������������������������������������������������������
             MOVE 1                  TO USER-ACTION USER-SETTING.
      *    �����������������������������������������������������������Ŀ
      *    �                    ESC, F1 to F10 keys                    �
      *    �������������������������������������������������������������
             MOVE ZERO               TO USER-NUMBER.
             MOVE 11                 TO USER-KEYS.
             CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    �����������������������������������������������������������Ŀ
      *    �                 PAGE-UP and PAGE-DOWN keys                �
      *    �������������������������������������������������������������
             MOVE 53                 TO USER-NUMBER.
             MOVE 2                  TO USER-KEYS.
             CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    �����������������������������������������������������������Ŀ
      *    �                 UP and DOWN ARROW keys                    �
      *    �������������������������������������������������������������
             MOVE 2                  TO USER-ACTION.
             MOVE 3                  TO USER-SETTING.
             MOVE 5                  TO USER-NUMBER.
             MOVE 2                  TO USER-KEYS.
             CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    �����������������������������������������������������������Ŀ
      *    �               A C T I V A T E   M O U S E                 �
      *    �������������������������������������������������������������
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
      *    �����������������������������������������������������������Ŀ
      *    �            Set MOUSE key to act as FUNCTION key           �
      *    �������������������������������������������������������������
             MOVE 3                  TO USER-ACTION.
             MOVE 27                 TO USER-NUMBER.
             MOVE 2                  TO USER-KEYS.
             CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.