@ECHO OFF
IF "%1"=="" GOTO NOPROG
SET COBDIR=C:\COBOL\BIN
..\BIN\COBOL %1;
PAUSE
DEL %1*.I*
GOTO END
:NOPROG
ECHO No program name specified
:END
