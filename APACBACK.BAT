@ECHO OFF
:: APACBACK.BAT
:: Do not change the name of this batch file
::
IF "%1==" GOTO USER

IF "%2==" GOTO ERROR

FOR %%S IN (B b) DO IF %1==/%%S GOTO DATA_BACK
FOR %%S IN (R r) DO IF %1==/%%S GOTO DATA_REST
GOTO ERROR

:COMP_BACK
CLS
ECHO.
ECHO.
ECHO.
ECHO.
ECHO Enter company to backup
ECHO.

FC CON NUL /LB1 /N | DATE | FIND "1: " > BAK_TEMP.BAT

ECHO @SET CDEPATH=%%5 > ENTER.BAT
CALL BAK_TEMP.BAT
DEL BAK_TEMP.BAT
DEL ENTER.BAT
IF NOT "%CDEPATH%==" CALL APACBACK /B %CDEPATH%
SET CDEPATH=
GOTO USER

:COMP_REST
CLS
ECHO.
ECHO.
ECHO.
ECHO.
ECHO Enter company to restore
ECHO.

FC CON NUL /LB1 /N | DATE | FIND "1: " > RES_TEMP.BAT

ECHO @SET CDEPATH=%%5 > ENTER.BAT
CALL RES_TEMP.BAT
DEL RES_TEMP.BAT
DEL ENTER.BAT
IF NOT "%CDEPATH%==" CALL APACBACK /R %CDEPATH%
SET CDEPATH=

:USER
::
::    Display a user menu and allow user to select action required
::
CLS
ECHO.
ECHO.
ECHO                       ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
ECHO                       ³ APAC Backup & Restore Manager ³ÜÜ
ECHO                       ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´ÛÛ
ECHO                       ³ 1. View Company Directories   ³ÛÛ
ECHO                       ³ 2. Backup Data Files          ³ÛÛ
ECHO                       ³ 3. Restore Data Files         ³ÛÛ
ECHO                       ³ 4. EXIT                       ³ÛÛ
ECHO                       ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙÛÛ
ECHO                         ÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛ
ECHO.
CHOICE /C1234 /N "                           Your choice: "
IF ERRORLEVEL 4 GOTO END
IF ERRORLEVEL 3 GOTO COMP_REST
IF ERRORLEVEL 2 GOTO COMP_BACK
IF ERRORLEVEL 1 GOTO USER_VIEW
GOTO USER

:DATA_BACK
::
:: Check to see if entered directory exists?
::
IF NOT EXIST %2_DATA\NUL GOTO NOCOMP

SET CMP=%2
::
:: This batch file assumes A: drive as the destination drive and if you
:: are using any other drive then change the following SET command to
:: SET FD=?, where ? is the drive letter to use as the destination.
::
SET FD=A
echo.
echo    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
echo    ³      APAC Accounting System - Backup Data Files                   ³
echo    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
echo    ³ This backup procedure uses pkzip to backup the data files and the ³
echo    ³ following parameters will be used. The data files that will be    ³
echo    ³ copied to the floppy/stiffy drive are for company code %CMP% and    ³
echo    ³ the destination drive is %FD%.                                       ³
echo    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
echo.
echo.
CHOICE /C:NY /N "      Correct Y/N? "
::
::  If correct? - Continue with backup
::
IF ERRORLEVEL 2 GOTO DO_BACKUP
::
::  If error? - Can it be corrected?
::
echo.
CHOICE /C:CE /N "    'C'hange destination drive or 'E'xit? "
::
:: EXIT?
::
IF ERRORLEVEL 2 GOTO END
::
::  Select destination drive
::
echo.
CHOICE /C:ABCDEFG /N "    Destination drive (A,B,C,D,E,F,G)? "
IF ERRORLEVEL 7 GOTO G_DRIVE
IF ERRORLEVEL 6 GOTO F_DRIVE
IF ERRORLEVEL 5 GOTO E_DRIVE
IF ERRORLEVEL 4 GOTO D_DRIVE
IF ERRORLEVEL 3 GOTO C_DRIVE
IF ERRORLEVEL 2 GOTO B_DRIVE
::
:: A drive
::
SET FD=A
GOTO DO_BACKUP

:G_DRIVE
SET FD=G
GOTO DO_BACKUP

:F_DRIVE
SET FD=F
GOTO DO_BACKUP

:E_DRIVE
SET FD=E
GOTO DO_BACKUP

:D_DRIVE
SET FD=D
GOTO DO_BACKUP

:C_DRIVE
SET FD=C
GOTO DO_BACKUP

:B_DRIVE
SET FD=B

:DO_BACKUP
cls
PKZIP -& %FD%:%CMP%BACK %CMP%_DATA\*.*
echo.
echo   ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
echo   ³ The backup of the data files is complete. Remove the diskette from ³
echo   ³ drive %FD%.                                                           ³
echo   ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
echo.
pause
GOTO END

:DATA_REST
::
:: Check to see if entered directory exists?
::
IF NOT EXIST %2_DATA\NUL GOTO NOCOMP
SET CMP=%2
::
:: This batch file assumes A: drive as the destination drive and if you
:: are using any other drive then change the following SET command to
:: SET FD=?, where ? is the drive letter to use as the destination.
::
SET FD=A
echo.
echo   ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
echo   ³     APAC Accounting System - Restore Data Files                    ³
echo   ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
echo   ³ This restore procedure uses pkunzip to restore the data files and  ³
echo   ³ the following parameters will be used. The data files that will be ³
echo   ³ copied from the floppy/stiffy drive are for company code %CMP% and   ³
echo   ³ the source drive is %FD%.                                             ³
echo   ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
echo.
echo.
CHOICE /C:NY /N "    Correct Y/N? "
::
::  If correct? - Continue with restore
::
IF ERRORLEVEL 2 GOTO DO_RESTORE
::
::  If error? - Can it be corrected?
::
echo.
CHOICE /C:CE /N "    'C'hange source drive or 'E'xit? "
::
:: EXIT?
::
IF ERRORLEVEL 2 GOTO END
::
::  Select source drive
::
echo.
CHOICE /C:ABCDEFG /N "    Source drive (A,B,C,D,E,F,G)? "
IF ERRORLEVEL 7 GOTO G_DRV
IF ERRORLEVEL 6 GOTO F_DRV
IF ERRORLEVEL 5 GOTO E_DRV
IF ERRORLEVEL 4 GOTO D_DRV
IF ERRORLEVEL 3 GOTO C_DRV
IF ERRORLEVEL 2 GOTO B_DRV
::
:: A drive
::
SET FD=A
GOTO DO_RESTORE

:G_DRV
SET FD=G
GOTO DO_RESTORE

:F_DRV
SET FD=F
GOTO DO_RESTORE

:E_DRV
SET FD=E
GOTO DO_RESTORE

:D_DRV
SET FD=D
GOTO DO_RESTORE

:C_DRV
SET FD=C
GOTO DO_RESTORE

:B_DRV
SET FD=B

cls
:DO_RESTORE
PKUNZIP -o %FD%:%CMP%BACK *.* %CMP%_DATA
echo.
echo   ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
echo   ³ The restore of the data files is complete. Remove the diskette from ³
echo   ³ drive %FD%.                                                            ³
echo   ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
echo.
pause
GOTO END

:NOCOMP
ECHO   Company code '%2' does not exist
PAUSE
GOTO END

:ERROR
ECHO Command-line Error: %0 %1 %2 %3 %4 %5
ECHO.

:HELP
ECHO.
ECHO     The APAC Backup Manager
ECHO.
ECHO        Usage:
ECHO.
ECHO          APACBACK                == Interactive mode
ECHO.
ECHO          APACBACK /B cpy ...     == Backup a company
ECHO.
ECHO          APACBACK /R cpy ...     == Restore a company
ECHO.
PAUSE

:USER_VIEW
cls
DIR ???_DATA. | MORE
PAUSE
GOTO USER

:END
