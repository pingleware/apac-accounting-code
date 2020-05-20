       IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     TESTCALL.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   MAY 2009.

                   COPYRIGHT NOTICE: COPYRIGHT (C) 2009 -
                                     by James William Lemmon.
                                       (Id No. 4412165050082).

                   All rights reserved.

                   e-mail jwlemmon@gmail.com.

000050 SECURITY.
                 This program is free software; you can redistribute
                 it and/or modify it under the terms of the GNU General
                 Public License as published by the Free Software
                 Foundation; either version 2 of the License, or (at
                 your option) any later version.

                 This program is distributed in the hope that it will
                 be useful, but WITHOUT ANY WARRANTY; without even the
                 implied warranty of MERCHANTABILITY or FITNESS FOR A
                 PARTICULAR PURPOSE.  See the GNU General Public License
                 for more details.

                 You should have received a copy of the GNU General
                 Public License along with this program; if not, write
                 to the Free Software Foundation, Inc., 59 Temple Place
                 - Suite 330, Boston, MA 02111-1307, USA.

000050 ENVIRONMENT DIVISION.
000060 CONFIGURATION SECTION.
000070 SPECIAL-NAMES.
000080                 CONSOLE IS CRT.
000090 INPUT-OUTPUT SECTION.
000100 FILE-CONTROL.

       working-storage section.
       77  WS-CHECK        PIC  X(18)    VALUE
                           "aeWlimemnomLalismJ".
       77  WS-PRG-NAME     PIC  X(28)    VALUE SPACES.
       77  WS-WINDOW-SAVE  PIC  X(10).
       77  WS-OPTION       PIC  X(01).

       COPY WS.WS.

       COPY FUNCTION.WS.

       LINKAGE SECTION.

       01  LS-CHAIN.
           03  LS-PRG-NAME     PIC  X(28).

      /
004580 SCREEN SECTION.
       01  S01-WINDOW.
           03  line 12 column 26
               VALUE "ÚÄÄÄÄÄProgram - TESTCALLÄÄÄÄÄ¿".
           03  line 13 column 26
               VALUE "³                            ³".
           03  line 14 column 26
               VALUE "³                            ³".
           03  line 15 column 26
               VALUE "³                            ³".
           03  line 16 column 26
               VALUE "ÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÙ".
001230 PROCEDURE DIVISION USING LS-CHAIN.

010250 AA000-MAIN      SECTION.
010260 AA000-INIT.

             Display "Testing TESTCALL - NetEXPRESS 5.1"
                     at line 03 column 29
                     with FOREGROUND-COLOR 7 HIGHLIGHT
                          BACKGROUND-COLOR 4.
             Display S01-WINDOW.
      *      Move LS-PRG-NAME    TO WS-PRG-NAME.
             Display WS-PRG-NAME AT line 14 column 27.
             Display "Press " at line 15 column 35
                     "ANY" with FOREGROUND-COLOR 6 HIGHLIGHT
                     " key".
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
             EXIT-PROGRAM.

