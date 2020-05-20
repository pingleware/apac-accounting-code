@ECHO OFF
IF "%1"=="" GOTO NOPROG
..\BIN\COBOL %1;
GOTO END
:NOPROG
ECHO No program name specified
:END
