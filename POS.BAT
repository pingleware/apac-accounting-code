rem copy pos\csh001.cbl
REM ..\BIN\COBOL CSH001,,CSH001.LST;
rem DEL CSH001.I*
rem del csh001.cbl
copy pos\csh002.cbl
C:\WINDOWS\MFENVG.EXE 32 COBOLW.EXE CSH002; > csh002.lst;
PAUSE
DEL CSH002.I*
del csh002.cbl
copy pos\csh003.cbl
C:\WINDOWS\MFENVG.EXE 32 COBOLW.EXE CSH003; > csh003.lst;
PAUSE
DEL CSH003.I*
del csh003.cbl
copy pos\cshbal.cbl
C:\WINDOWS\MFENVG.EXE 32 COBOLW.EXE CSHBAL; > cshbal.lst;
PAUSE
DEL CSHBAL.I*
del cshbal.cbl
copy pos\cshend.cbl
C:\WINDOWS\MFENVG.EXE 32 COBOLW.EXE CSHEND; > cshend.lst;
PAUSE
DEL CSHEND.I*
del cshend.cbl
copy pos\cshflt.cbl
C:\WINDOWS\MFENVG.EXE 32 COBOLW.EXE CSHFLT; > cshflt.lst;
PAUSE
DEL CSHFLT.I*
del cshflt.cbl
copy pos\cshint.cbl
C:\WINDOWS\MFENVG.EXE 32 COBOLW.EXE CSHINT; > cshint.lst;
PAUSE
DEL CSHINT.I*
del cshint.cbl
copy pos\cstats.cbl
C:\WINDOWS\MFENVG.EXE 32 COBOLW.EXE CSTATS; > cstats.lst;
PAUSE
DEL CSTATS.I*
del cstats.cbl
copy pos\opndrw.cbl
C:\WINDOWS\MFENVG.EXE 32 COBOLW.EXE OPNDRW; > opndrw.lst;
PAUSE
DEL OPNDRW.I*
del opndrw.cbl
copy pos\pos001.cbl
C:\WINDOWS\MFENVG.EXE 32 COBOLW.EXE POS001; > pos001.lst;
PAUSE
DEL POS001.I*
del pos001.cbl
copy pos\pos004.cbl
C:\WINDOWS\MFENVG.EXE 32 COBOLW.EXE POS004; > pos004.lst;
PAUSE
DEL POS004.I*
del pos004.cbl
copy pos\posupd.cbl
C:\WINDOWS\MFENVG.EXE 32 COBOLW.EXE POSUPD; > posupd.lst;
PAUSE
DEL POSUPD.I*
del posupd.cbl
copy csh002.g* c:\apac32\pos
copy csh003.g* c:\apac32\pos
copy cshbal.g* c:\apac32\pos
copy cshend.g* c:\apac32\pos
copy cshflt.g* c:\apac32\pos
copy cshint.g* c:\apac32\pos
copy cstats.g* c:\apac32\pos
copy opndrw.g* c:\apac32\pos
copy pos001.g* c:\apac32\pos
copy pos004.g* c:\apac32\pos
copy posupd.g* c:\apac32\pos
del csh002.g*
del csh003.g*
del cshbal.g*
del cshend.g*
del cshflt.g*
del cshint.g*
del cstats.g*
del opndrw.g*
del pos001.g*
del pos004.g*
del posupd.g*
