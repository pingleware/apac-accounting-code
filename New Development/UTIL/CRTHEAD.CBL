      $set linkcount"384" GNT "CRTHEAD.GNT"
      ******************************************************************
      *                                                                *
      *  ******   *******  ********  **   **  ******    ***    ******  *
      * **    **  **    **    **     **   **  **       ** **   **   ** *
      * **        **    **    **     **   **  **      **   **  **   ** *
      * **        ******      **     *******  ****    **   **  **   ** *
      * **        **   **     **     **   **  **      *******  **   ** *
      * **    **  **    **    **     **   **  **      **   **  **   ** *
      *  ******   **    **    **     **   **  ******  **   **  ******  *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     S E T U P   S C R E E N   H E A D I N G S                  *
      *                                                                *
      *       Version 9.04.04 - February/March 2018                    *
      *                                                                *
      ******************************************************************
      *                                                                *
      *  This program has been introduced to ensure that all screens   *
      *  in the system have the same format and reduce the maintenance *
      *  in future. The system has had a major change to the screen    *
      *  size from 25 lines to 50 lines and a format change which this *
      *  program will handle.                                          *
      *  All programs will now call this program to setup and display  *
      *  the screen layouts.                                           *
      *                                                                *
      *  P A R A M T E R S:                                            *
      *                                                                *
      *  Used by calling program (WORKING-STORAGE) and this program    * 
      *  (LINKAGE SECTION) for Main screen layout and headings.        *       
      *                                                                *
      * 01  CRT-HEADINGS.                                              *
      *                                                                *
      *    TYPE:                                                       *
      *       C = Clear comment line (50)                              *
      *       E = Clear lines (any line or lines between 5 and 46)     *
      *       H = Change heading                                       *
      *       S = Clear the screen and display headings                *
      *       T = Change the time                                      *
      *                                                                *
      *     03  CRT-TYPE            PIC  X(01).                        *
      *                                                                *
      *    The following two fields are used for clearing a portion of *
      *    the screen. (Start line and end line)                       *
      *                                                                *
      *     03  CRT-START           PIC  9(02).                        *
      *     03  CRT-END             PIC  9(02).                        *
      *                                                                *
      *    Calling Program                                             *
      *                                                                *
      *     03  CRT-PROGRAM         PIC  X(15).                        *
      *                                                                *
      *    Screen Heading                                              *
      *                                                                *
      *     03  CRT-HEADING         PIC  X(40).                        *
      *                                                                *
      *    Company Name                                                *
      *                                                                *
      *     03  CRT-COMPANY         PIC  X(40).                        *
      *                                                                *
      *    WorkStation / Supervisor                                    *
      *                                                                *
      *     03  CRT-TERMINAL.                                          *
      *         05  CRT-WRKHD       PIC  X(11).                        *
      *         05  CRT-WRKST       PIC  X(03).                        *
      *                                                                *
      *  This will also allow for the next phase where a graphics      *
      *  interface will be introduced.                                 *
      *                                                                *
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID.  CRTHEAD.
       AUTHOR.         J W LEMMON (APAC).
       DATE-WRITTEN.   NOVEMBER 2012.

                COPYRIGHT NOTICE: COPYRIGHT (C) 2012 - 2018
                                  by James William Lemmon.
                                    (Id No. 4412165050082).

                All rights reserved.

                e-mail jwlemmon@gmail.com.

       SECURITY.
                This program is free software; you can redistribute
                it and/or modify it under the terms of the GNU General
                Public License as published by the Free Software
                Foundation; either version 3 of the License, or (at
                your option) any later version.

                This program is distributed in the hope that it will
                be useful, but WITHOUT ANY WARRANTY; without even the
                implied warranty of MERCHANTABILITY or FITNESS FOR A
                PARTICULAR PURPOSE. See the GNU General Public License
                for more details.

                You should have received a copy of the GNU General
                Public License along with this program. If not, see
                <http://www.gnu.org/licenses/>.

       INSTALLATION.          APAC.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                         CURSOR IS CSTART
                         CONSOLE IS CRT
                         CRT STATUS IS KEY-STATUS.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

      *
      *         **        **    *****    ******    **   **
      *         **        **   **   **   **   **   **  **
      *         **        **   **   **   **  **    ** **
      *         **   **   **   **   **   *****     ****
      *          ** **** **    **   **   **  **    ** **
      *           ***  ***     **   **   **   **   **  **
      *            *    *       *****    **   **   **   **
      *
       WORKING-STORAGE SECTION.
       77  WS-COPYRIGHT            PIC  X(18) VALUE "aeWlimemnomLalismJ".
       77  WS-S1                   PIC  9(04) COMP-5.
       77  WS-S2                   PIC  9(04) COMP-5.

       COPY "WS.WS".

       01  W05-HEADINGS.
           03  W05-TIME.
               05  W05-ETIME       PIC  99/99/9999.
           03  W05-DTIME                       REDEFINES W05-TIME.
               05  W05-HHMMSS      PIC  X(08).
               05  FILLER          PIC  X(02).    
           03  W05-PROGRAM         PIC  X(15).
           03  W05-USER            PIC  X(20).
           03  W05-HEADING         PIC  X(40).
           03  W05-COMPANY         PIC  X(40).
           03  W05-WORK-STATION.
               05  W05-WRKHD       PIC  X(11).
               05  W05-WRKST       PIC  X(03).

       01  W50-JUSTIFY.
           03  W50-DATA.
               05  W50-CCHAR       PIC  X(01)  OCCURS 40.
           03  W50-DATA-LEFT                   REDEFINES W50-DATA.
               05  FILLER          PIC  X(01).
               05  W50-D39         PIC  X(39).
           03  W50-DATA-15                     REDEFINES W50-DATA.
               05  W50-15.
                   07  FILLER      PIC  X(01).
                   07  W50-14      PIC  X(14).
               05  FILLER          PIC  X(25).
           03  W50-DATA-20                     REDEFINES W50-DATA.
               05  W50-20.
                   07  FILLER      PIC  X(01).
                   07  W50-19      PIC  X(19).
               05  FILLER          PIC  X(20).
           03  W50-DATA-30                     REDEFINES W50-DATA.
               05  W50-30.
                   07  FILLER      PIC  X(01).
                   07  W50-29      PIC  X(29).
               05  FILLER          PIC  X(10).

      *
      *    **       ******  **    **  **   **    ***     *****   ******
      *    **         **    ***   **  **  **    ** **   **   **  **
      *    **         **    ****  **  ** **    **   **  **       ** 
      *    **         **    ** ** **  ****     *******  **       *****
      *    **         **    **  ****  ** **    **   **  **  ***  **  
      *    **         **    **   ***  **  **   **   **  **   **  **
      *    *******  ******  **    **  **   **  **   **   *****   ******
      *
       LINKAGE SECTION.

       COPY "USER.LS".

       COPY "HEADING.CRT".

      * 
      *   *****    *****   ******   ******  ******  **    **
      *  **   **  **   **  **   **  **      **      ***   **
      *  **       **       **  **   **      **      ****  **
      *   *****   **       *****    *****   *****   ** ** **
      *       **  **       **  **   **      **      **  ****
      *  **   **  **   **  **   **  **      **      **   ***
      *   *****    *****   **   **  ******  ******  **    **
      *
       SCREEN SECTION.

       COPY "Blank.crt".

      *
      *      ******   ******    *****    *****   ******  ******   **   **  ******    ****** 
      *      **   **  **   **  **   **  **   **  **      **   **  **   **  **   **   **
      *      **   **  **  **   **   **  **       **      **   **  **   **  **  **    **
      *      ******   *****    **   **  **       *****   **   **  **   **  *****     *****
      *      **       **  **   **   **  **       **      **   **  **   **  **  **    **
      *      **       **   **  **   **  **   **  **      **   **  **   **  **   **   **
      *      **       **   **   *****    *****   ******  ******    *****   **   **   ******
      *
       PROCEDURE DIVISION USING CRT-HEADINGS LS-USER-ID.
       A00-MAIN SECTION.
       AA00.
             ACCEPT WS-TIME FROM TIME.
             MOVE WS-TIME            TO W05-ETIME.
             INSPECT W05-TIME REPLACING ALL "/" BY ":".
             MOVE CRT-TERMINAL       TO W05-WORK-STATION.
      *
      *    **** The following routine is performed each time this
      *         program is called, in case any of the variables have
      *         been changed by the calling program.
      *       
             PERFORM BA000-JUSTIFY.
                                
       AA05.
             EVALUATE CRT-TYPE
               WHEN "4"         MOVE 4601      TO CPOS
                                DISPLAY CLR-LINE AT CPOS
                                DISPLAY CLEAR-L50
                                
               WHEN "C"         DISPLAY CLEAR-L50
               
               WHEN "E"         IF CRT-START < 5
                                    MOVE 5     TO CRT-START
                                END-IF
                                IF CRT-END > 46
                                    MOVE 46    TO CRT-END
                                END-IF
                                MOVE CRT-START TO CLIN
                                MOVE 01        TO CCOL
      *
      *    **** The following test allows for clearing just one line on
      *         the screen. This test will also trap a potential crash
      *         if the program is called with End pointer less than the
      *         starting pointer.
      *                              
                                IF NOT(CRT-END > CRT-START)
                                    DISPLAY CLR-LINE AT CPOS
                                ELSE    
                                    PERFORM TEST AFTER UNTIL CLIN = CRT-END
                                        DISPLAY CLR-LINE AT CPOS
                                        ADD 1  TO CLIN
                                    END-PERFORM
                                END-IF    
                                DISPLAY CLR-COMPANY-HEAD
                                
               WHEN "H"         DISPLAY CLR-COMPANY-HEAD
                                DISPLAY CLR-SCREEN-HEAD
                                DISPLAY CLR-VERSION-HEAD
                                
               WHEN "S"         DISPLAY CLR-SCREEN
                                
               WHEN "T"         DISPLAY CLR-TIME
             END-EVALUATE.
             MOVE SPACE              TO CRT-TYPE.
             MOVE 5                  TO CRT-START.
             MOVE 46                 TO CRT-END.

       AA09.
             EXIT PROGRAM.

       BA000-JUSTIFY SECTION.
       BA00.
             MOVE CRT-HEADING        TO W50-DATA.
             MOVE 40                 TO WS-S1.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³               FIND THE LENGTH OF THE HEADING              ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       BA05.
           IF W50-CCHAR(WS-S1) = SPACE
               SUBTRACT 1            FROM WS-S1
               GO TO BA05.
             SUBTRACT WS-S1 FROM 40  GIVING WS-COUNT.
           IF WS-COUNT > 2
               DIVIDE 2              INTO WS-COUNT
               SUBTRACT WS-COUNT FROM 40 GIVING WS-S2
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³         CENTRE THE HEADING IN THE DISPLAY WINDOW          ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
               PERFORM TEST AFTER UNTIL WS-S1 = 0
                   MOVE W50-CCHAR(WS-S1) TO W50-CCHAR(WS-S2)
                   MOVE SPACE        TO W50-CCHAR(WS-S1)
                   SUBTRACT 1        FROM WS-S1 WS-S2
               END-PERFORM.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³          MOVE CENTERED HEADING TO DISPLAY FIELD           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
             MOVE W50-DATA           TO W05-HEADING.

      *BA09.
      *      EXIT.

       BA10.
             MOVE CRT-COMPANY        TO W50-DATA.
             MOVE 40                 TO WS-S1.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³           FIND THE LENGTH OF THE COMPANY NAME             ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       BA15.
           IF W50-CCHAR(WS-S1) = SPACE
               SUBTRACT 1            FROM WS-S1
               GO TO BA15.
             SUBTRACT WS-S1 FROM 40  GIVING WS-COUNT.
           IF WS-COUNT > 2
               DIVIDE 2              INTO WS-COUNT
               SUBTRACT WS-COUNT FROM 40 GIVING WS-S2
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³           CENTRE COMPANY IN THE DISPLAY WINDOW            ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
               PERFORM TEST AFTER UNTIL WS-S1 = 0
                   MOVE W50-CCHAR(WS-S1) TO W50-CCHAR(WS-S2)
                   MOVE SPACE        TO W50-CCHAR(WS-S1)
                   SUBTRACT 1        FROM WS-S1 WS-S2
               END-PERFORM.
             MOVE W50-DATA           TO W05-COMPANY.

       BA20.
             MOVE CRT-PROGRAM        TO W50-DATA.
             MOVE 15                 TO WS-S1.

       BA25.
           IF W50-CCHAR(WS-S1) = SPACE
               SUBTRACT 1            FROM WS-S1
               GO TO BA25.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                RIGHT JUSTIFY PROGRAM NAME                 ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
           IF WS-S1 < 15
               MOVE 15               TO WS-S2
               PERFORM TEST AFTER UNTIL WS-S1 = 0
                   MOVE W50-CCHAR(WS-S1) TO W50-CCHAR(WS-S2)
                   MOVE SPACE        TO W50-CCHAR(WS-S1)
                   SUBTRACT 1        FROM WS-S1 WS-S2
               END-PERFORM
               MOVE W50-15           TO W05-PROGRAM.

       BA999.
             EXIT.
