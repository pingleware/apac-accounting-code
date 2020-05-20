copy apac\apacmenu.cbl
C:\WINDOWS\MFENVG.EXE 32 COBOLW.EXE APACMENU; > APACMENU.LST
PAUSE
DEL APACMENU.I*
DEL APACMENU.cbl
copy apac\apacterm.cbl
C:\WINDOWS\MFENVG.EXE 32 COBOLW.EXE APACTERM; > APACTERM.LST
PAUSE
DEL APACTERM.I*
DEL APACterm.cbl
copy menu\bup000.cbl
C:\WINDOWS\MFENVG.EXE 32 COBOLW.EXE BUP000; > BUP000.LST;
PAUSE
DEL BUP000.I*
DEL bup000.cbl
copy menu\bup001.cbl
C:\WINDOWS\MFENVG.EXE 32 COBOLW.EXE BUP001; > BUP001.LST;
PAUSE
DEL BUP001.I*
DEL bup001.cbl
copy menu\nopmenu.cbl
C:\WINDOWS\MFENVG.EXE 32 COBOLW.EXE NOPMENU; > NOPMENU.LST;
PAUSE
DEL NOPMENU.I*
DEL nopmenu.cbl
copy menu\repmenu.cbl
C:\WINDOWS\MFENVG.EXE 32 COBOLW.EXE REPMENU; > REPMENU.LST;
PAUSE
DEL REPMENU.I*
DEL repmenu.cbl
copy apacmenu.g* c:\apac32
copy apacterm.g* c:\apac32
copy bup000.g* c:\apac32\mnu
copy bup001.g* c:\apac32\mnu
copy nopmenu.g* c:\apac32\mnu
copy repmenu.g* c:\apac32\mnu
del apacmenu.g*
del apacterm.g*
del bup000.g*
del bup001.g*
del nopmenu.g*
del repmenu.g*
