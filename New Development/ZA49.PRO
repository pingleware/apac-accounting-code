      *    *************************************************************
      *    ****   F I L E   L I M I T	E X C E E D E D
      *          (Config.sys Dos/Win/Windows 95/98/NT/2000)
      *          (Config.nt Windows XP)
      *          (System.ini Windows.me)
      *    *************************************************************
       ZA49.
             DISPLAY "Too many files OPEN"                                                AT 1008 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             DISPLAY "Check the FILES option in CONFIG.SYS (Dos,Windows 3/95/98/NT/2000)" AT 1108.
             DISPLAY "CONFIG.NT  (Windows XP/VISTA/7)"                                    AT 1234.
             DISPLAY "SYSTEM.INI (Windows ME)"                                            AT 1334.
             DISPLAY "Press any key to continue"                                          AT 1408.
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
             GO TO ZA205.
