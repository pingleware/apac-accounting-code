      $SET LINKCOUNT"256" GNT"USR001.GNT"
      ******************************************************************
      *                                                                *
      *   **    **   ******   *******     ****      ****       **      *
      *   **    **  **    **  **    **   **  **    **  **     ***      *
      *   **    **  **        **   **   **    **  **    **     **      *
      *   **    **   ******   *****     **    **  **    **     **      *
      *   **    **        **  **  **    **    **  **    **     **      *
      *   **    **  **    **  **   **    **  **    **  **      **      *
      *    ******    ******   **    **   ****       ****     ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     U S E R   L O G I N   A N D   L O G O U T   P R O G R A M  *
      *                                                                *
      *       Version 9.00.82 - May 2016                               *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     USR001.
       AUTHOR.         J W LEMMON (APAC).
       DATE-WRITTEN.   FEBRUARY 1994.

                COPYRIGHT NOTICE: COPYRIGHT (C) 1983 - 2016
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

       COPY "SYSUSE.SL".

       DATA DIVISION.
      *
      *          ********  ********  **        ******** 
      *          **           **     **        **
      *          **           **     **        ** 
      *          *****        **     **        *****
      *          **           **     **        **  
      *          **           **     **        **
      *          **        ********  ********  ********
      *
       FILE SECTION.

       COPY "SYSUSE.FD".

      *
      *         **         **    ******    *******    **    **
      *         **         **   **    **   **    **   **   **
      *         **    *    **   **    **   **   **    **  **
      *         **   ***   **   **    **   ******     *****
      *          ** ** ** **    **    **   **   **    **  **
      *           ***   ***     **    **   **    **   **   **
      *            *     *       ******    **    **   **    **
      *
       WORKING-STORAGE SECTION.
       77  WS-CHECK                PIC  X(18)    VALUE "aeWlimemnomLalismJ".
       77  WS-S1                   PIC  9(04)    COMP-5.
       77  WS-S2                   PIC  9(04)    COMP-5.
       77  WS-S3                   PIC  9(04)    COMP-5.
       77  WS-S4                   PIC  9(04)    COMP-5.
       77  WS-SPC                  PIC  X(40) VALUE SPACES.
       77  WS-OPTION               PIC X(01).
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Used by calling program (WORKING-STORAGE) and called program ³
      *    ³ (LINKAGE SECTION) for Main screen layout and headings.       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       01  CRT-HEADINGS.
      *
      *    TYPE:
      *         C =  Clear comment line (50)
      *         E =  Clear lines (any line or lines between 5 and 46)
      *         H =  Change heading
      *         S =  Clear the screen
      *
           03  CRT-TYPE            PIC  X(01).
      *
      *    The following two fields are used for clearing a portion of
      *    the screen. (Start line and end line)
      *
           03  CRT-START           PIC  9(02).
           03  CRT-END             PIC  9(02).
      *
      *    Calling Program
      *
           03  CRT-PROGRAM         PIC  X(15) VALUE "USR001".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "UTILITIES - USER MAINTENANCE".
      *
      *    Company Name
      *
           03  CRT-COMPANY         PIC  X(40).
      *
      *    WorkStation / Supervisor
      *
           03  CRT-TERMINAL.
               05  CRT-WRKHD       PIC  X(11).
               05  CRT-WRKST       PIC  X(03).

       COPY "WS.WS".

       COPY "CHAIN.WS".

       01  W10-USER-FIELDS.
           03  W10-USER            PIC  9(03) VALUE ZERO.
           03  W10-PASS            PIC  X(08) VALUE SPACES.
           03  W10-ENCRYPT                    REDEFINES W10-PASS.
               05  W10-ENC1        PIC  X(01) COMP-X.
               05  W10-ENC2        PIC  X(01) COMP-X.
               05  W10-ENC3        PIC  X(01) COMP-X.
               05  W10-ENC4        PIC  X(01) COMP-X.
               05  W10-ENC5        PIC  X(01) COMP-X.
               05  W10-ENC6        PIC  X(01) COMP-X.
               05  W10-ENC7        PIC  X(01) COMP-X.
               05  W10-ENC8        PIC  X(01) COMP-X.
           03  W10-NAME.
               05  W10-N1-2        PIC  X(02).
               05  W10-N3-4        PIC  X(02).
               05  W10-N5-6        PIC  X(02).
               05  W10-N7-8        PIC  X(02).
               05  W10-N9-10       PIC  X(02).
               05  W10-N11-12      PIC  X(02).
               05  W10-N13-14      PIC  X(02).
               05  W10-N15-16      PIC  X(02).
               05  W10-N17-18      PIC  X(02).
               05  W10-N19-20      PIC  X(02).
               05  W10-N21-22      PIC  X(02).
               05  W10-N23-24      PIC  X(02).
               05  W10-N25-26      PIC  X(02).
               05  W10-N27-28      PIC  X(02).
               05  W10-N29-30      PIC  X(02).

       01 W15-SECURITY.
           03  W15-NO              PIC  9(03) VALUE ZERO.
           03  W15-STATION         PIC  X(03) VALUE SPACES.
           03  W15-NAME            PIC  X(30) VALUE SPACES.
           03  W15-ENCRYPT                    REDEFINES W15-NAME.
               05  W15-N1-2        PIC  X(02).
               05  W15-N3-4        PIC  X(02).
               05  W15-N5-6        PIC  X(02).
               05  W15-N7-8        PIC  X(02).
               05  W15-N9-10       PIC  X(02).
               05  W15-N11-12      PIC  X(02).
               05  W15-N13-14      PIC  X(02).
               05  W15-N15-16      PIC  X(02).
               05  W15-N17-18      PIC  X(02).
               05  W15-N19-20      PIC  X(02).
               05  W15-N21-22      PIC  X(02).
               05  W15-N23-24      PIC  X(02).
               05  W15-N25-26      PIC  X(02).
               05  W15-N27-28      PIC  X(02).
               05  W15-N29-30      PIC  X(02).
           03  W15-PASS            PIC  X(08) VALUE SPACES.
           03  W15-DATE            PIC  9(06) VALUE ZERO.
           03  W15-GL              PIC  X(01) VALUE "N".
           03  W15-GLLEV           PIC  9(01) VALUE ZERO.
           03  W15-STOCK           PIC  X(01) VALUE "N".
           03  W15-STLEV           PIC  9(01) VALUE ZERO.
           03  W15-DEBT            PIC  X(01) VALUE "N".
           03  W15-DBLEV           PIC  9(01) VALUE ZERO.
           03  W15-CRED            PIC  X(01) VALUE "N".
           03  W15-CRLEV           PIC  9(01) VALUE ZERO.
           03  W15-PARM            PIC  X(01) VALUE "N".
           03  W15-PALEV           PIC  9(01) VALUE ZERO.
           03  W15-SALES           PIC  X(01) VALUE "Y".
           03  W15-SALEV           PIC  9(01) VALUE ZERO.
           03  W15-JOBS            PIC  X(01) VALUE "N".
           03  W15-JBLEV           PIC  9(01) VALUE ZERO.
           03  W15-WAGES           PIC  X(01) VALUE "N".
           03  W15-WGLEV           PIC  9(01) VALUE ZERO.
           03  W15-ADM             PIC  X(01) VALUE SPACES.
           03  W15-DIV             PIC  9(03) VALUE ZERO.
           03  W15-SP              PIC  9(02) VALUE ZERO.
           03  W15-ORD             PIC  9(06) COMP-5.
           03  FILLER              PIC  X(03).

       01  W20-TEMP.
           03  W20-PASS            PIC  X(08).

       01  BLOCK-DETAIL1.
           03  ORIGINAL-VID1.
               05  ORIGINAL-CHAR1  PIC  X(01) OCCURS 80.
           03  REVERSE-VID1.
               05  REVERSE-CHAR1   PIC  X(01) OCCURS 80.

       01  SHADOW-DETAIL1.
           03  SHAD1-ROW           PIC  9(02) COMP-X.
           03  SHAD1-COL           PIC  9(02) COMP-X.
           03  SHAD1-LINES         PIC  9(02) COMP-X.
           03  SHAD1-WIDTH         PIC  9(02) COMP-X.
           03  SHAD1-CHAR          PIC  X(01).

       01  CRT-DETAIL1.
           03  TOP-ROW1            PIC  9(02) COMP-X.
           03  BOTTOM-ROW1         PIC  9(02) COMP-X.
           03  STRING-LENGTH1      PIC  9(04) COMP-X.
           03  SCREEN-POSITION1.
               05  SCREEN-LIN1     PIC  9(02) COMP-X.
               05  SCREEN-COL1     PIC  9(02) COMP-X.

       01  BLOCK-DETAIL2.
           03  ORIGINAL-VID2.
               05  ORIGINAL-CHAR2  PIC  X(01) OCCURS 80.
           03  REVERSE-VID2.
               05  REVERSE-CHAR2   PIC  X(01) OCCURS 80.

       01  SHADOW-DETAIL2.
           03  SHAD2-ROW           PIC  9(02) COMP-X.
           03  SHAD2-COL           PIC  9(02) COMP-X.
           03  SHAD2-LINES         PIC  9(02) COMP-X.
           03  SHAD2-WIDTH         PIC  9(02) COMP-X.
           03  SHAD2-CHAR          PIC  X(01).

       01  CRT-DETAIL2.
           03  TOP-ROW2            PIC  9(02) COMP-X.
           03  BOTTOM-ROW2         PIC  9(02) COMP-X.
           03  STRING-LENGTH2      PIC  9(04) COMP-X.
           03  SCREEN-POSITION2.
               05  SCREEN-LIN2     PIC  9(02) COMP-X.
               05  SCREEN-COL2     PIC  9(02) COMP-X.

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       COPY "W50.WS".

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

       COPY "CHAIN.LS".

       COPY "FILE.IDS".

      *
      *   ******    ******   *******   ********  ********  **     **
      *  **    **  **    **  **    **  **        **        ***    **
      *  **        **        **    **  **        **        ** *   **
      *   ******   **        ******    *****     *****     **  *  **
      *        **  **        **   **   **        **        **   * **
      *  **    **  **    **  **    **  **        **        **    ***
      *   ******    ******   **    **  ********  ********  **     **
      *

       SCREEN SECTION.

       01  MENU-INSTRUCT.
           03  BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
               05  LINE 50 COLUMN  2 VALUE "Use ".
               05          COLUMN  6 VALUE     ""                                                               FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN  7 VALUE       " or ".
               05          COLUMN 11 VALUE           ""                                                          FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 12 VALUE             " to point to an item, and ".
               05          COLUMN 38 VALUE                                       "<Enter>"                         FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 45 VALUE                                              " to Select ".
               05          COLUMN 56 VALUE                                                         "<Esc>"         FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 61 VALUE                                                              " to Exit".

       01  S02.
           03  BACKGROUND-COLOR Green FOREGROUND-COLOR Black.
               05  LINE  8 COLUMN  7 VALUE "                    ".
               05  LINE  9 COLUMN  7 VALUE "    USER CONTROL    " FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE 10 COLUMN  7 VALUE " 1"                   FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN  9 VALUE   " Login            ".
               05  LINE 11 COLUMN  7 VALUE " 2"                   FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN  9 VALUE   " Logout           ".
               05  LINE 12 COLUMN  7 VALUE " 3"                   FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN  9 VALUE   " Change Password  ".
               05  LINE 13 COLUMN  7 VALUE " 4"                   FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN  9 VALUE   " Add User         ".
               05  LINE 14 COLUMN  7 VALUE " 5"                   FOREGROUND-COLOR Grey HIGHLIGHT  .
               05          COLUMN  9 VALUE   " Amend User Rec.  ".
               05  LINE 15 COLUMN  7 VALUE " 6"                   FOREGROUND-COLOR Grey HIGHLIGHT .
               05          COLUMN  9 VALUE   " Delete User      ".
               05  LINE 16 COLUMN  7 VALUE " 0"                   FOREGROUND-COLOR Grey HIGHLIGHT .
               05          COLUMN  9 VALUE   " Exit             ".
               05  LINE 17 COLUMN  7 VALUE "                    " FOREGROUND-COLOR Green HIGHLIGHT.

       01  S10.
           03  BACKGROUND-COLOR Blue FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE  6 COLUMN 38 VALUE                          "LOG-IN".
           03  BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey.           
               05  LINE  8 COLUMN 13 VALUE " User Number                                            " HIGHLIGHT.
               05  LINE  9 COLUMN 13 VALUE " Password                                               " HIGHLIGHT.
           03  BACKGROUND-COLOR Green FOREGROUND-COLOR Black.  
               05  LINE 12 COLUMN 13 VALUE "                                                        ".
               05  LINE 13 COLUMN 13 VALUE " The USER NUMBER must  be a  valid  user number  in the ".
               05  LINE 14 COLUMN 13 VALUE " range 001 to 999. The  PASSWORD  must  be the password ".
               05  LINE 15 COLUMN 13 VALUE " that has been assigned to the user number selected.    ".
               05  LINE 16 COLUMN 13 VALUE " D O  N O T  press  the  enter  key after keying in the ".
               05  LINE 17 COLUMN 13 VALUE " user number. If the user  number is less than 100, key ".
               05  LINE 18 COLUMN 13 VALUE " in the number and press the TAB key. The system is set ".
               05  LINE 19 COLUMN 13 VALUE " up with USER NUMBER 001 and the password will be setup ".
               05  LINE 20 COLUMN 13 VALUE " as PASSWORD and this should be changed by you.         ".
               05  LINE 21 COLUMN 13 VALUE " SEE option 'L' on the program control menu.            ".
               05  LINE 22 COLUMN 13 VALUE "                                                        ".

       01  S10-NO.
           03  LINE  8 COLUMN 26 PIC ZZ9 USING W10-USER BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT AUTO.

       01  S10-PASS.
           03  LINE  9 COLUMN 26 PIC X(08) USING W10-PASS BACKGROUND-COLOR Magenta FOREGROUND-COLOR Black AUTO SECURE.

       01  S11.
           03  LINE  6 COLUMN 32 VALUE "CREATE USER RECORD" FOREGROUND-COLOR Grey HIGHLIGHT.

       01  S11A.
           03  LINE  6 COLUMN 32 VALUE "AMEND USER RECORD" FOREGROUND-COLOR Grey HIGHLIGHT.

       01  S12.
           03  LINE  6 COLUMN 32 VALUE "DELETE USER RECORD" FOREGROUND-COLOR Grey HIGHLIGHT.

       01  S13.
           03  BACKGROUND-COLOR Grey FOREGROUND-COLOR Blue.
               05  LINE  8 COLUMN  4 VALUE "                                                                        ".
               05  LINE  9 COLUMN  4 VALUE " User Number                                                            ".
               05  LINE 10 COLUMN  4 VALUE " User Name                                                              ".
               05  LINE 11 COLUMN  4 VALUE " Password                                                               ".
               05  LINE 12 COLUMN  4 VALUE " Salesperson No.                                                        ".
               05  LINE 13 COLUMN  4 VALUE "                 Module   Security Level                                ".
               05  LINE 14 COLUMN  4 VALUE " Sales                                                                  ".
               05  LINE 15 COLUMN  4 VALUE " Debtors                                                                ".
               05  LINE 16 COLUMN  4 VALUE " Creditors                                                              ".
               05  LINE 17 COLUMN  4 VALUE " Stock                                                                  ".
               05  LINE 18 COLUMN  4 VALUE " Job Costing                                                            ".
               05  LINE 19 COLUMN  4 VALUE " General Ledger                                                         ".
               05  LINE 20 COLUMN  4 VALUE " Parameters                                                             ".
               05  LINE 21 COLUMN  4 VALUE " Wages                                                                  ".
               05  LINE 22 COLUMN  4 VALUE " Module must be a Y or N! (Any key other than Y will be changed to N)   ".
               05  LINE 23 COLUMN  4 VALUE " Security level 0 through 9. 0=Enquiries, 1=Create, 2=Amend, 3=Limited  " FOREGROUND-COLOR Red.
               05  LINE 24 COLUMN  4 VALUE " Transactions and Reports, 4=All Reports, Transactions, Delete etc      " FOREGROUND-COLOR Red.
               05  LINE 25 COLUMN  4 VALUE " 5-8=Reserved, 9=Supervisor (Any level includes lower levels)           " FOREGROUND-COLOR Red.
               05  LINE 26 COLUMN  4 VALUE " eg. Level 2 includes levels 0 and 1                                    " FOREGROUND-COLOR Red.
               05  LINE 27 COLUMN  4 VALUE " NB level 1 includes sales. Level 4 includes period ends etc            ".
               05  LINE 28 COLUMN  4 VALUE "                                                                        ".
           03  BACKGROUND-COLOR Black.
               05  LINE  9 COLUMN 21 VALUE                 "   ".
               05  LINE 10 COLUMN 21 VALUE                 "                              ".
               05  LINE 11 COLUMN 21 VALUE                 "        ".  
               05  LINE 12 COLUMN 21 VALUE                 "  ".
       01  S14.
           03  BACKGROUND-COLOR Black FOREGROUND-COLOR Grey HIGHLIGHT.
               05  S14A.
                   07  LINE  9 COLUMN 21 PIC  ZZ9   USING W15-NO    AUTO.
               05  S14B.
                   07  LINE 10 COLUMN 21 PIC  X(30) USING W15-NAME  AUTO.
                   07  LINE 11 COLUMN 21 PIC  X(08) USING W15-PASS  AUTO.
                   07  LINE 12 COLUMN 21 PIC  9(02) USING W15-SP    AUTO.
                   07  LINE 14 COLUMN 24 PIC  X(01) USING W15-SALES AUTO.
                   07          COLUMN 36 PIC  9(01) USING W15-SALEV AUTO.
                   07  LINE 15 COLUMN 24 PIC  X(01) USING W15-DEBT  AUTO.
                   07          COLUMN 36 PIC  9(01) USING W15-DBLEV AUTO.
                   07  LINE 16 COLUMN 24 PIC  X(01) USING W15-CRED  AUTO.
                   07          COLUMN 36 PIC  9(01) USING W15-CRLEV AUTO.
                   07  LINE 17 COLUMN 24 PIC  X(01) USING W15-STOCK AUTO.
                   07          COLUMN 36 PIC  9(01) USING W15-STLEV AUTO.
                   07  LINE 18 COLUMN 24 PIC  X(01) USING W15-JOBS  AUTO.
                   07          COLUMN 36 PIC  9(01) USING W15-JBLEV AUTO.
                   07  LINE 19 COLUMN 24 PIC  X(01) USING W15-GL    AUTO.
                   07          COLUMN 36 PIC  9(01) USING W15-GLLEV AUTO.
                   07  LINE 20 COLUMN 24 PIC  X(01) USING W15-PARM  AUTO.
                   07          COLUMN 36 PIC  9(01) USING W15-PALEV AUTO.
                   07  LINE 21 COLUMN 24 PIC  X(01) USING W15-WAGES AUTO.
                   07          COLUMN 36 PIC  9(01) USING W15-WGLEV AUTO.

       01  SD-CHG-PW.
           03  BACKGROUND-COLOR Brown FOREGROUND-COLOR Brown.
               05  LINE 10 COLUMN 29 VALUE "                       ".
               05  LINE 11 COLUMN 29 VALUE " Password              " HIGHLIGHT.
               05  LINE 12 COLUMN 29 VALUE " New Password          " HIGHLIGHT.
               05  LINE 13 COLUMN 29 VALUE "                       ".

       01  SA-CHG-PW.
           03  BACKGROUND-COLOR Brown FOREGROUND-COLOR Grey HIGHLIGHT.
               05  COLUMN 43 PIC  X(08) USING W10-PASS AUTO SECURE.
      *
      *        *******   *******    ******    ******   ********  *******   **     **  *******    ******** 
      *        **    **  **    **  **    **  **    **  **        **    **  **     **  **    **   **
      *        **    **  **   **   **    **  **        **        **    **  **     **  **   **    **
      *        *******   ******    **    **  **        *****     **    **  **     **  ******     *****
      *        **        **   **   **    **  **        **        **    **  **     **  **   **    **
      *        **        **    **  **    **  **    **  **        **    **  **     **  **    **   **
      *        **        **    **   ******    ******   ********  *******    *******   **    **   ********
      *
       PROCEDURE DIVISION USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY DATA-FILE-IDS.
       AA000-MAIN              SECTION.
       AA00.
             MOVE LS-PARID           TO WS-PARID.
             MOVE LS-USER-ID         TO WS-USER-ID.
             MOVE LS0-SECURITY       TO WS0-SECURITY.
             MOVE LS-COMPANY         TO CRT-COMPANY.
             MOVE LS-TERMINAL        TO CRT-TERMINAL.
             PERFORM CHANGE-HEADING.      
             PERFORM ZA000-INIT.
           IF WS0-NAME = "ANYBODY"
               PERFORM CA000
               GO TO AA90.
           IF MOUSE-ATTACHED = "Y"
               MOVE 66               TO MOUSE-FUNC
               MOVE 1                TO MOUSE-PARAM
               CALL X"AF" USING MOUSE-FUNC MOUSE-PARAM
               END-IF.
             PERFORM BA000.
           IF MOUSE-ATTACHED = "Y"
               MOVE 66               TO MOUSE-FUNC
               MOVE 0                TO MOUSE-PARAM
               CALL X"AF" USING MOUSE-FUNC MOUSE-PARAM.
       AA90.
             MOVE WS0-SECURITY       TO LS0-SECURITY.
             CLOSE SYSUSER.

       AA99.
             EXIT PROGRAM.

       COPY "AA900.ALM".
             
      *
      *    ****    READ FILES ROUTINES
      *
       AC000              SECTION.

       COPY "SYSUSE.RD".

      *
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
       AD000             SECTION.

       COPY "SYSUSE.WR".

       COPY "FUNCTION.SCR".

       COPY "ERROR.SCR".

       COPY "OPTION.CRT".

       COPY "CLEAR.CRT".

       COPY "LOCKED.REC".

       BA000        SECTION.
       BA00.
             MOVE 8                  TO SHADE-ROW.
             MOVE 7                  TO SHADE-COL.
             MOVE 20                 TO SHADE-WIDTH.
             MOVE 10                 TO SHADE-LINES.
             DISPLAY S02.
             DISPLAY MENU-INSTRUCT.
             PERFORM SCREEN-SHADOW.

       BA02.
             MOVE 9                  TO TOP-ROW.
             MOVE 15                 TO BOTTOM-ROW.
             MOVE 8                  TO SCREEN-LIN.
             MOVE 8                  TO SCREEN-COL.
             MOVE 17                 TO STRING-LENGTH.
             PERFORM SAVE-ATTR.

       BA05.
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       BA10.
           IF ADIS-FUNC
               EVALUATE KEY-CODE-1
                 WHEN UP-KEY     PERFORM MOVE-BLOCK-UP
                 WHEN DOWN-KEY   PERFORM MOVE-BLOCK-DOWN
                 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
                                 IF MOUSE-ENTER = "Y"
                                     MOVE "N"  TO MOUSE-ENTER
                                     GO TO BA15
                                 END-IF
                                 IF MOUSE-Y = 24
                                     IF MOUSE-X = 5
                                         MOVE 5  TO KEY-CODE-1
                                         GO TO BA10
                                     ELSE
                                     IF MOUSE-X = 10
                                         MOVE 6  TO KEY-CODE-1
                                         GO TO BA10
                                     ELSE
                                     IF MOUSE-X > 36 AND < 44
                                         MOVE 0  TO KEY-CODE-1
                                         GO TO BA10
                                     ELSE
                                     IF MOUSE-X > 54 AND < 60
                                         MOVE 0  TO KEY-CODE-1
                                         MOVE 1  TO KEY-TYPE
                                         GO TO BA10
                                     END-IF
                                 END-IF
                 WHEN ENTER-KEY  GO TO BA15
                 WHEN OTHER      PERFORM AA900-ALARM
               END-EVALUATE
               GO TO BA05
           ELSE
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    MOVE "0"  TO WS-OPTION
                                 GO TO BA18
                 WHEN OTHER      PERFORM AA900-ALARM
               END-EVALUATE
               GO TO BA05
           ELSE
           IF DATA-8BIT
               MOVE KEY-CODE-1X      TO WS-OPTION
               IF (WS-OPTION < 0 OR > 6)
                   PERFORM AA900-ALARM
                   GO TO BA05
               END-IF
               GO TO BA16
           ELSE
               PERFORM AA900-ALARM
               GO TO BA05.

       BA15.
             EVALUATE SCREEN-LIN
               WHEN 9            MOVE "1"        TO WS-OPTION
               
               WHEN 10           MOVE "2"        TO WS-OPTION
               
               WHEN 11           MOVE "3"        TO WS-OPTION
               
               WHEN 12           MOVE "4"        TO WS-OPTION
               
               WHEN 13           MOVE "5"        TO WS-OPTION
               
               WHEN 14           MOVE "6"        TO WS-OPTION
               
               WHEN 15           MOVE "0"        TO WS-OPTION
               
               WHEN OTHER        GO TO BA05
             END-EVALUATE.
           GO TO BA18.

       BA16.
             PERFORM CLEAR-BLOCK.
             EVALUATE WS-OPTION
               WHEN "0"          MOVE 15         TO SCREEN-LIN
               
               WHEN "1"          MOVE 9          TO SCREEN-LIN
               
               WHEN "2"          MOVE 10         TO SCREEN-LIN
               
               WHEN "3"          MOVE 11         TO SCREEN-LIN
               
               WHEN "4"          MOVE 12         TO SCREEN-LIN
               
               WHEN "5"          MOVE 13         TO SCREEN-LIN
               
               WHEN "6"          MOVE 14         TO SCREEN-LIN
               
             END-EVALUATE.
             PERFORM SAVE-ATTR.
             PERFORM MARK-BLOCK.

       BA18.
             PERFORM SAVE-SCREEN.
             MOVE BLOCK-DETAIL       TO BLOCK-DETAIL1.
             MOVE SHADOW-DETAIL      TO SHADOW-DETAIL1.
             MOVE CRT-DETAIL         TO CRT-DETAIL1.
             EVALUATE WS-OPTION
               WHEN "0"     GO TO BA999
               WHEN "1"     PERFORM CA000
                            GO TO BA999
               WHEN "2"     PERFORM DA000
                            GO TO BA999
               WHEN "3"     PERFORM EA000
               WHEN "4"     PERFORM FA000
               WHEN "5"     PERFORM GA000
               WHEN "6"     PERFORM HA000
               WHEN OTHER   GO TO BA05
             END-EVALUATE.
             PERFORM RESTORE-SCREEN.
             MOVE BLOCK-DETAIL1      TO BLOCK-DETAIL.
             MOVE SHADOW-DETAIL1     TO SHADOW-DETAIL.
             MOVE CRT-DETAIL1        TO CRT-DETAIL.
             GO TO BA05.

       BA999.
             EXIT.

       BB000       SECTION.
       BB00.
             MOVE W10-N1-2           TO W15-N29-30.
             MOVE W10-N3-4           TO W15-N1-2.
             MOVE W10-N5-6           TO W15-N27-28
             MOVE W10-N7-8           TO W15-N3-4
             MOVE W10-N9-10          TO W15-N25-26.
             MOVE W10-N11-12         TO W15-N5-6.
             MOVE W10-N13-14         TO W15-N23-24.
             MOVE W10-N15-16         TO W15-N7-8.
             MOVE W10-N17-18         TO W15-N21-22.
             MOVE W10-N19-20         TO W15-N9-10.
             MOVE W10-N21-22         TO W15-N19-20.
             MOVE W10-N23-24         TO W15-N11-12.
             MOVE W10-N25-26         TO W15-N17-18.
             MOVE W10-N27-28         TO W15-N13-14.
             MOVE W10-N29-30         TO W15-N15-16.

       BB05.
             EXIT.

       BB010        SECTION.
       BB10.
             MOVE W15-N29-30         TO W10-N1-2.
             MOVE W15-N1-2           TO W10-N3-4.
             MOVE W15-N27-28         TO W10-N5-6.
             MOVE W15-N3-4           TO W10-N7-8.
             MOVE W15-N25-26         TO W10-N9-10.
             MOVE W15-N5-6           TO W10-N11-12.
             MOVE W15-N23-24         TO W10-N13-14.
             MOVE W15-N7-8           TO W10-N15-16.
             MOVE W15-N21-22         TO W10-N17-18.
             MOVE W15-N9-10          TO W10-N19-20.
             MOVE W15-N19-20         TO W10-N21-22.
             MOVE W15-N11-12         TO W10-N23-24.
             MOVE W15-N17-18         TO W10-N25-26.
             MOVE W15-N13-14         TO W10-N27-28.
             MOVE W15-N15-16         TO W10-N29-30.

       BB15.
             EXIT.

       BB020        SECTION.
       BB20.
             ADD 31                  TO W10-ENC1.
             ADD 13                  TO W10-ENC2.
             ADD 32                  TO W10-ENC3.
             ADD 23                  TO W10-ENC4.
             ADD 34                  TO W10-ENC5.
             ADD 43                  TO W10-ENC6.
             ADD 35                  TO W10-ENC7.
             ADD 53                  TO W10-ENC8.

       BB25.
             EXIT.

      *
      *    ****    L O G I N
      *
       CA000        SECTION.
       CA00.
             PERFORM ERASE-SCREEN.
             MOVE 8                  TO SHADE-ROW.
             MOVE 13                 TO SHADE-COL.
             MOVE 56                 TO SHADE-WIDTH.
             MOVE 2                  TO SHADE-LINES.
             DISPLAY S10.
             PERFORM SCREEN-SHADOW.
             MOVE 12                 TO SHADE-ROW.
             MOVE 13                 TO SHADE-COL.
             MOVE 56                 TO SHADE-WIDTH.
             MOVE 11                 TO SHADE-LINES.
             PERFORM SCREEN-SHADOW.
             MOVE ZERO               TO W10-USER.
             MOVE SPACES             TO W10-PASS.

       CA05.
             ACCEPT S10-NO.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    MOVE ZERO  TO W10-USER
                 WHEN OTHER      PERFORM AA900-ALARM
               END-EVALUATE
               GO TO CA05.
           IF W10-USER = ZERO
               GO TO CA999.
             MOVE W10-USER           TO USE-NO.
             PERFORM READ-SYSUSER THRU READ-SYSUSER-EXIT.
      *      PERFORM CLEAR-L50.
           IF WS-F-ERROR = 64
               MOVE "Invalid USER number" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO CA05.
             MOVE USE-NAME           TO W15-NAME.
             PERFORM BB010.
             DISPLAY W10-NAME AT 0830 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.

       CA10.
             ACCEPT S10-PASS.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    MOVE ZERO  TO W10-USER
                                 GO TO CA999
                 WHEN OTHER      PERFORM AA900-ALARM
               END-EVALUATE
               GO TO CA10.
             MOVE 8                  TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W10-PASS BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 1                  TO WS-LENGTH.
             MOVE W10-PASS           TO W20-PASS.
             PERFORM BB020.
           IF (W10-PASS NOT = USE-PASS) AND (W20-PASS NOT = "<,>.?/:;")
               MOVE "Invalid Password" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO CA999.
             MOVE USE-RECORD         TO WS0-SECURITY.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ WS0-DIV Will be used to pass the G/L division between the ³
      *    ³ programs. Used by consolidated G/L Systems.        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
             MOVE ZERO               TO WS0-DIV.
             MOVE WS-USER            TO USE-STATION.
             PERFORM REWRITE-SYSUSER-UNLOCK THRU WRITE-SYSUSER-EXIT.

       CA999.
             EXIT.
      *
      *    ****    L O G O U T
      *
       DA000        SECTION.
       DA00.
           IF WS0-NO = ZERO
               GO TO DA999.
             MOVE WS0-NO             TO USE-NO.
             PERFORM READ-SYSUSER-LOCK THRU READ-SYSUSER-EXIT.
             MOVE SPACES             TO USE-STATION.
             PERFORM REWRITE-SYSUSER-UNLOCK THRU WRITE-SYSUSER-EXIT.
             MOVE "ANYBODY"          TO WS0-NAME.
             MOVE SPACES             TO WS0-PASS.
             MOVE "N"                TO WS0-GL WS0-STOCK WS0-DEBT WS0-CRED WS0-PARM WS0-JOBS WS0-WAGES.
             MOVE "Y"                TO WS0-SALES.
             MOVE ZERO               TO WS0-GLLEV WS0-STLEV WS0-DBLEV WS0-CRLEV WS0-PALEV WS0-SALEV WS0-JBLEV WS0-WGLEV.

       DA999.
             EXIT.
      *
      *    ****    C H A N G E   P A S S W O R D
      *
       EA000        SECTION.
       EA00.
           IF WS0-NO = ZERO
               GO TO EA999.
             PERFORM ERASE-SCREEN.
             DISPLAY "CHANGE PASSWORD" AT 0633 WITH FOREGROUND-COLOR Grey HIGHLIGHT.
             MOVE "Are you sure (Y/N)" TO WS-OPT-MES.
             MOVE "1"                TO WS-INSTR.
             MOVE SPACE              TO WS-OPTION.
             PERFORM OPT-MESSAGE.
           IF WS-OPTION = "N"
               GO TO EA999.
             MOVE 10                 TO SHADE-ROW.
             MOVE 29                 TO SHADE-COL.
             MOVE 23                 TO SHADE-WIDTH.
             MOVE 4                  TO SHADE-LINES.
             DISPLAY SD-CHG-PW.
             PERFORM SCREEN-SHADOW.
             MOVE WS0-NO             TO USE-NO.
             PERFORM READ-SYSUSER-LOCK THRU READ-SYSUSER-EXIT.
             MOVE SPACES             TO W10-PASS.
           
       EA10.
             ACCEPT SA-CHG-PW AT LINE 11.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
                     UNLOCK SYSUSER
                     GO TO EA999
                 WHEN OTHER  PERFORM AA900-ALARM
               END-EVALUATE
               GO TO EA10.
             MOVE 8                  TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W10-PASS BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 1                  TO WS-LENGTH.
             PERFORM BB020.
           IF (W10-PASS NOT = USE-PASS) AND (W20-PASS NOT = "<,>.?/:;")
               MOVE "Invalid Password" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               UNLOCK SYSUSER
               GO TO EA999.
             MOVE SPACES             TO W10-PASS.
           
       EA15.
             ACCEPT SA-CHG-PW AT LINE 12.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
                     UNLOCK SYSUSER
                     GO TO EA999
                 WHEN OTHER  PERFORM AA900-ALARM
               END-EVALUATE
               GO TO EA10.
             MOVE 8   TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W10-PASS BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 1                  TO WS-LENGTH.
             PERFORM BB020.
             MOVE W10-PASS           TO USE-PASS WS0-PASS.
             PERFORM REWRITE-SYSUSER-UNLOCK THRU WRITE-SYSUSER-EXIT.

       EA999.
             EXIT.
      *
      *    ****    A D D   U S E R S
      *
       FA000        SECTION.
       FA00.
           IF WS0-NO NOT = 001
               GO TO FA999.
             INITIALIZE W15-SECURITY.
             MOVE 1                  TO W15-ORD.
             PERFORM ERASE-SCREEN.
             DISPLAY S11.
             MOVE 8                  TO SHADE-ROW.
             MOVE 4                  TO SHADE-COL.
             MOVE 72                 TO SHADE-WIDTH.
             MOVE 21                 TO SHADE-LINES.
             DISPLAY S13.
             PERFORM SCREEN-SHADOW.

       FA05.
             ACCEPT S14A.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    GO TO FA999
                 WHEN OTHER      PERFORM AA900-ALARM
               END-EVALUATE
               GO TO FA05.
      *      PERFORM CLEAR-L50.
             MOVE W15-NO             TO USE-NO.
             PERFORM READ-SYSUSER THRU READ-SYSUSER-EXIT.
           IF WS-F-ERROR = ZERO
               MOVE "User number exists" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO FA05.
               
       FA10.
             ACCEPT S14B.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    GO TO FA999
                 WHEN OTHER      PERFORM AA900-ALARM
               END-EVALUATE
               GO TO FA10.
           IF W15-NAME = SPACES OR W15-PASS = SPACES OR W15-STOCK = SPACES OR W15-DEBT = SPACES OR W15-GL    = SPACES OR   
              W15-CRED = SPACES OR W15-PARM = SPACES OR W15-SALES = SPACES OR W15-JOBS = SPACES OR W15-WAGES = SPACES
               MOVE "Data incomplete" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO FA05.
           IF W15-SP > 40
               MOVE 40               TO W15-SP.
             MOVE 30                 TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W15-NAME BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 8                  TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W15-PASS BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 1                  TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W15-GL BY VALUE WS-LENGTH RETURNING WS-STATUS.
           IF NOT(W15-GL = "N" OR "Y")
               MOVE "N"              TO W15-GL.
             CALL "CBL_TOUPPER" USING W15-STOCK BY VALUE WS-LENGTH RETURNING WS-STATUS.
           IF NOT(W15-STOCK = "N" OR "Y")
               MOVE "N"              TO W15-STOCK.
             CALL "CBL_TOUPPER" USING W15-DEBT BY VALUE WS-LENGTH RETURNING WS-STATUS.
           IF NOT(W15-DEBT = "N" OR "Y")
               MOVE "N"              TO W15-DEBT.
             CALL "CBL_TOUPPER" USING W15-CRED BY VALUE WS-LENGTH RETURNING WS-STATUS.
           IF NOT (W15-CRED = "N" OR "Y")
               MOVE "N"              TO W15-CRED.
             CALL "CBL_TOUPPER" USING W15-PARM BY VALUE WS-LENGTH RETURNING WS-STATUS.
           IF NOT(W15-PARM = "N" OR "Y")
               MOVE "N"              TO W15-PARM.
             CALL "CBL_TOUPPER" USING W15-SALES BY VALUE WS-LENGTH RETURNING WS-STATUS.
           IF NOT(W15-SALES = "N" OR "Y")
               MOVE "N"              TO W15-SALES.
             CALL "CBL_TOUPPER" USING W15-JOBS BY VALUE WS-LENGTH RETURNING WS-STATUS.
           IF NOT(W15-JOBS = "N" OR "Y")
               MOVE "N"              TO W15-JOBS.
             CALL "CBL_TOUPPER" USING W15-WAGES BY VALUE WS-LENGTH RETURNING WS-STATUS.
           IF NOT(W15-WAGES = "N" OR "Y")
               MOVE "N"              TO W15-WAGES.
           IF W15-GLLEV > 4 AND < 9
               MOVE 4                TO W15-GLLEV.
           IF W15-STLEV > 4 AND < 9
               MOVE 4                TO W15-STLEV.
           IF W15-DBLEV > 5 AND < 9
               MOVE 5                TO W15-DBLEV.
           IF W15-CRLEV > 4 AND < 9
               MOVE 4                TO W15-CRLEV.
           IF W15-PALEV > 4 AND < 9
               MOVE 4                TO W15-PALEV.
           IF W15-SALEV > 4 AND < 9
               MOVE 4                TO W15-SALEV.
           IF W15-JBLEV > 4 AND < 9
               MOVE 4                TO W15-JBLEV.
           IF W15-WGLEV > 4 AND < 9
               MOVE 4                TO W15-WGLEV.
             MOVE W15-PASS           TO W10-PASS.
             PERFORM BB020.
             MOVE W10-PASS           TO W15-PASS.
             MOVE W15-NAME           TO W10-NAME.
             PERFORM BB000.
             MOVE W15-SECURITY       TO USE-RECORD.
             PERFORM WRITE-SYSUSER THRU WRITE-SYSUSER-EXIT.
             MOVE SPACES             TO W15-NAME W15-PASS.

       FA999.
             EXIT.
      *
      *    ****    A M E N D   U S E R S
      *
       GA000        SECTION.
       GA00.
           IF WS0-NO NOT = 001
               GO TO GA999.
             PERFORM ERASE-SCREEN.
             DISPLAY S11A.
             MOVE 8                  TO SHADE-ROW.
             MOVE 4                  TO SHADE-COL.
             MOVE 72                 TO SHADE-WIDTH.
             MOVE 21                 TO SHADE-LINES.
             DISPLAY S13.
             PERFORM SCREEN-SHADOW.

       GA02.
             ACCEPT S14A.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    GO TO GA999
                 WHEN OTHER      PERFORM AA900-ALARM
               END-EVALUATE
               GO TO GA02.
      *      PERFORM CLEAR-L50.
             MOVE W15-NO             TO USE-NO.
             PERFORM READ-SYSUSER-LOCK THRU READ-SYSUSER-EXIT.
           IF WS-F-ERROR = 64
               MOVE "User number invalid" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO GA02.
             MOVE USE-RECORD         TO W15-SECURITY.
             MOVE W15-PASS           TO W10-PASS.
             SUBTRACT 31             FROM W10-ENC1.
             SUBTRACT 13             FROM W10-ENC2.
             SUBTRACT 32             FROM W10-ENC3.
             SUBTRACT 23             FROM W10-ENC4.
             SUBTRACT 34             FROM W10-ENC5.
             SUBTRACT 43             FROM W10-ENC6.
             SUBTRACT 35             FROM W10-ENC7.
             SUBTRACT 53             FROM W10-ENC8.
             MOVE W10-PASS           TO W15-PASS.
             PERFORM BB010.
             MOVE W10-NAME           TO W15-NAME.

       GA05.
             ACCEPT S14B.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
                     UNLOCK SYSUSER
                     GO TO GA999
                 WHEN OTHER  PERFORM AA900-ALARM
               END-EVALUATE
               GO TO GA05.
      *      PERFORM CLEAR-L50.
           IF W15-NAME = SPACES OR W15-PASS = SPACES OR W15-DEBT = SPACES OR W15-STOCK = SPACES OR W15-GL   = SPACES OR 
              W15-PARM = SPACES OR W15-JOBS = SPACES OR W15-CRED = SPACES OR W15-WAGES = SPACES OR W15-SALES = SPACES
               MOVE "Data incomplete" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO GA05.
           IF W15-SP > 40
               MOVE 40               TO W15-SP.
             MOVE 30                 TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W15-NAME BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 8                  TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W15-PASS BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 1                  TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W15-GL BY VALUE WS-LENGTH RETURNING WS-STATUS.
           IF NOT(W15-GL = "N" OR "Y")
               MOVE "N"              TO W15-GL.
             CALL "CBL_TOUPPER" USING W15-STOCK BY VALUE WS-LENGTH RETURNING WS-STATUS.
           IF NOT(W15-STOCK = "N" OR "Y")
               MOVE "N"              TO W15-STOCK.
             CALL "CBL_TOUPPER" USING W15-DEBT BY VALUE WS-LENGTH RETURNING WS-STATUS.
           IF NOT(W15-DEBT = "N" OR "Y")
               MOVE "N"              TO W15-DEBT.
             CALL "CBL_TOUPPER" USING W15-CRED BY VALUE WS-LENGTH RETURNING WS-STATUS.
           IF NOT(W15-CRED = "N" OR "Y")
               MOVE "N"              TO W15-CRED.
             CALL "CBL_TOUPPER" USING W15-PARM BY VALUE WS-LENGTH RETURNING WS-STATUS.
           IF NOT(W15-PARM = "N" OR "Y")
               MOVE "N"              TO W15-PARM.
             CALL "CBL_TOUPPER" USING W15-SALES BY VALUE WS-LENGTH RETURNING WS-STATUS.
           IF NOT(W15-SALES = "N" OR "Y")
               MOVE "N"   TO W15-SALES.
             CALL "CBL_TOUPPER" USING W15-JOBS BY VALUE WS-LENGTH RETURNING WS-STATUS.
           IF NOT(W15-JOBS = "N" OR "Y")
               MOVE "N"              TO W15-JOBS.
             CALL "CBL_TOUPPER" USING W15-WAGES BY VALUE WS-LENGTH RETURNING WS-STATUS.
           IF NOT(W15-WAGES = "N" OR "Y")
               MOVE "N"              TO W15-WAGES.
           IF W15-GLLEV > 4 AND < 9
               MOVE 4                TO W15-GLLEV.
           IF W15-STLEV > 4 AND < 9
               MOVE 4                TO W15-STLEV.
           IF W15-DBLEV > 5 AND < 9
               MOVE 5                TO W15-DBLEV.
           IF W15-CRLEV > 4 AND < 9
               MOVE 4                TO W15-CRLEV.
           IF W15-PALEV > 4 AND < 9
               MOVE 4                TO W15-PALEV.
           IF W15-SALEV > 4 AND < 9
               MOVE 4                TO W15-SALEV.
           IF W15-JBLEV > 4 AND < 9
               MOVE 4                TO W15-JBLEV.
           IF W15-WGLEV > 4 AND < 9
               MOVE 4                TO W15-WGLEV.
             MOVE W15-PASS           TO W10-PASS.
             PERFORM BB020.
             MOVE W10-PASS           TO W15-PASS.
             MOVE W15-NAME           TO W10-NAME.
             PERFORM BB000.
             MOVE W15-SECURITY       TO USE-RECORD.
             PERFORM REWRITE-SYSUSER-UNLOCK THRU WRITE-SYSUSER-EXIT.
             MOVE SPACES             TO W15-NAME W15-PASS.

       GA999.
             EXIT.
      *
      *    ****    D E L E T E   U S E R S
      *
       HA000        SECTION.
       HA00.
           IF WS0-NO NOT = 001
               GO TO HA999.
             PERFORM ERASE-SCREEN.
             DISPLAY S12.
             MOVE 8                  TO SHADE-ROW.
             MOVE 4                  TO SHADE-COL.
             MOVE 72                 TO SHADE-WIDTH.
             MOVE 21                 TO SHADE-LINES.
             DISPLAY S13.
             PERFORM SCREEN-SHADOW.

       HA05.
             ACCEPT S14A.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
                     GO TO HA999
                 WHEN OTHER  PERFORM AA900-ALARM
               END-EVALUATE
               GO TO HA05.
             MOVE W15-NO             TO USE-NO.
             PERFORM READ-SYSUSER-LOCK THRU READ-SYSUSER-EXIT.
           IF WS-F-ERROR = 64
               MOVE "Invalid user number" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO HA05.
      *      PERFORM CLEAR-L50.
             MOVE USE-RECORD         TO W15-SECURITY.
             PERFORM BB010.
             MOVE W10-NAME           TO W15-NAME.
             DISPLAY S14.

       HA10.
             MOVE "Delete this user (Y/N)  [ ]" TO WS-OPT-MES.
             MOVE "1"                TO WS-INSTR.
             MOVE SPACE              TO WS-OPTION.
             PERFORM OPT-MESSAGE.
           IF WS-OPTION = "N"
               GO TO HA999.
             PERFORM DELETE-SYSUSER-REC THRU WRITE-SYSUSER-EXIT.

       HA999.
             EXIT.
      /    *************************************************************
      *   I N I T I A L I S E   P R O G R A M
      *    *************************************************************
       ZA000-INIT        SECTION.
       ZA000-OPEN.
      *
      *    ****    D O   N O T   D I S P L A Y   E R R O R
      *                        M E S S A G E S
      *
             MOVE 44                 TO USER-NUMBER.
             MOVE 1                  TO USER-SETTING.
             CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

             COPY "FUNCTION.PRO".

       ZA05.
             OPEN I-O SYSUSER.
           IF WS-STATUS = "05"
               CLOSE SYSUSER
               OPEN OUTPUT SYSUSER
               INITIALIZE USE-RECORD
               MOVE 001              TO USE-NO
               MOVE "ADMINISTRATOR"
            TO W10-NAME
               PERFORM BB000
               MOVE W15-NAME         TO USE-NAME
               MOVE "oNsjyzuy"       TO USE-PASS
               MOVE "Y"              TO USE-GL USE-STOCK USE-DEBT USE-CRED USE-PARM USE-SALES USE-JOBS USE-WAGES
               MOVE 9                TO USE-GLLEV USE-STLEV USE-DBLEV USE-CRLEV USE-PALEV USE-SALEV USE-JBLEV USE-WGLEV
               MOVE 1                TO USE-ORD
               WRITE USE-RECORD
               CLOSE SYSUSER
               GO TO ZA05.
             GO TO ZA999.

       ZA999.
             EXIT.

       I-O-ERRORS      SECTION.

       COPY "ERRORS.PRO".

       DISPLAY-FILE-NAME.
             MOVE ZERO               TO WS-KEY.
             EVALUATE WS-F-ERROR
               WHEN 64         MOVE W02-SYSUSER   TO WS-FILE
                               MOVE USE-NO        TO WS-KEY
               WHEN OTHER      MOVE "ERROR"       TO WS-FILE
                               MOVE "PRP064"      TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
