      $set LINKCOUNT"384" GNT"STKCON.GNT"
      ******************************************************************
      *                                                                *
      *    ******   ********  **    **   ******    ******   **     **  *
      *   **    **     **     **   **   **    **  **    **  ***    **  *
      *   **           **     **  **    **        **    **  ** *   **  *
      *    ******      **     *****     **        **    **  **  *  **  *
      *         **     **     **  **    **        **    **  **   * **  *
      *   **    **     **     **   **   **    **  **    **  **    ***  *
      *    ******      **     **    **   ******    ******   **     **  *
      *                                                                *
      *   ENGLISH                                                      *
      *                                                                *
      *   S T O C K   F I L E   C O N V E R T   F I L E   V E R  6.75  *
      *                                                                *
      *     VERSION 8.09 - July 2003                                   *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     STKCON.
       AUTHOR.         J W LEMMON (APAC).
       DATE-WRITTEN.   MAY 1994.

                COPYRIGHT NOTICE: COPYRIGHT (C) 1994 - 2017
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
                      CONSOLE IS CRT.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY "CONTROL.SL".

       COPY "PARAM.SL".

       COPY "STOCK.SL".

          SELECT STKOLD  ASSIGN DISK
                          LOCK EXCLUSIVE
                          STATUS WS-STATUS
                          ACCESS SEQUENTIAL
                          ORGANIZATION SEQUENTIAL.

       COPY "SPARTS.SL".

          SELECT SPAOLD  ASSIGN DISK
                          LOCK EXCLUSIVE
                          STATUS WS-STATUS
                          ACCESS SEQUENTIAL
                          ORGANIZATION SEQUENTIAL.

       COPY "STKMEM.SL".

          SELECT STMOLD  ASSIGN DISK
                          LOCK EXCLUSIVE
                          STATUS WS-STATUS
                          ACCESS SEQUENTIAL
                          ORGANIZATION SEQUENTIAL.
      /
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

       COPY "CONTROL.FDE".

       COPY "PARAM.FDE".

       COPY "STOCK.FDE".

       FD  STKOLD    LABEL RECORD STANDARD
                     VALUE OF FILE-ID W02-STKOLD
                     RECORDING MODE IS VARIABLE.
       01  STO-RECORD1.
           03  STO-CODE        PIC  X(14).
           03  STO-ACODE       PIC  X(10).
           03  STO-BIN         PIC  X(03).
           03  STO-LEDG        PIC  9(03)    COMP.
           03  STO-DESC.
               05 STO-DKEY     PIC  X(14).
               05 FILLER       PIC  X(16).
           03  STO-QUANT       PIC S9(07)V9(04) COMP-3.
           03  STO-LEVEL       PIC S9(07)V99 COMP-3.
           03  STO-COST        PIC S9(06)V999 COMP-3.
           03  STO-SELL        PIC S9(07)V99 COMP-3.
           03  STO-CASH        PIC S9(07)V99 COMP-3.
           03  STO-WSALE       PIC S9(07)V99 COMP-3.
           03  STO-AVGE        PIC S9(06)V999 COMP-3.
           03  STO-O-AVGE REDEFINES STO-AVGE
           PIC S9(07)V99 COMP-3.
           03  STO-ORD        PIC S9(07)V99 COMP-3.
           03  STO-LTIME       PIC  9(01)V99 COMP-3.
           03  STO-MTD         PIC S9(09)V99 COMP-3.
           03  STO-MTDC        PIC S9(09)V99 COMP-3.
           03  STO-MTDV        PIC S9(09)V99 COMP-3.
           03  STO-YTD         PIC S9(09)V99 COMP-3.
           03  STO-YTDC        PIC S9(09)V99 COMP-3.
           03  STO-YTDV        PIC S9(09)V99 COMP-3.
           03  STO-MTH1        PIC S9(09)V99 COMP-3.
           03  STO-MTH2        PIC S9(09)V99 COMP-3.
           03  STO-MTH3        PIC S9(09)V99 COMP-3.
           03  STO-MAVG        PIC S9(09)V99 COMP-3.
           03  STO-MARKUP      PIC S9(03)V99 COMP-3.
           03  STO-CMARKUP     PIC S9(03)V99 COMP-3.
           03  STO-WMARKUP     PIC S9(03)V99 COMP-3.
           03  STO-ACT         PIC  9(06)    COMP-3.
           03  STO-PUR         PIC  9(06)    COMP-3.
           03  STO-DTE         PIC  9(06)    COMP-3.
           03  STO-DISC        PIC  X(01).
           03  STO-IND         PIC  9(01).
           03  STO-SUPP        PIC  X(05).
           03  STO-PRC.
               05  STO-PACK    PIC  9(03)V99 COMP-3.
           03  STO-GUAR        PIC  9(02)    COMP-5.
           03  STO-TAX         PIC  9(01).
           03  STO-XREF        PIC  X(14).

      /
       FD  SPAOLD    LABEL RECORD STANDARD
                     VALUE OF FILE-ID W02-SPAOLD
                     RECORDING MODE IS VARIABLE.
       01  PRO-REC1.
           03  PRO-ITEM        PIC X(14).
           03  PRO-COMMENT     PIC X(40).
           03  PRO-TYPE        PIC 9(02)      COMP.
           03  PRO-PARTS OCCURS 30.
               05  PRO-CODE     PIC X(14).
               05  PRO-QUANT    PIC 9(05)V9999 COMP-3.

       COPY "SPARTS.FD".

       FD  STMOLD    LABEL RECORD STANDARD
                     VALUE OF FILE-ID W02-STMOLD
                     RECORDING MODE IS VARIABLE.
       01  SMO-RECORD1.
           03  SMO-KEY.
               05  SMO-CODE    PIC  X(14).
               05  SMO-SEQ     PIC  9(02).
           03  SMO-DATA        PIC  X(60).

       COPY "STKMEM.FDE".

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
       77  WS-CHECK                PIC  X(18)           VALUE "aeWlimemnomLalismJ".
       77  WS-S1     PIC 9(04)   COMP-5.
       77  WS-SUB           PIC 9(04)     COMP-5.
       77  WS-NETKEY        PIC 9(04)     COMP-5 VALUE 1.
       77  WS-PARKEY     PIC 9(04)   COMP-5 VALUE 1.
       77  WS-ERROR         PIC 9(01)     VALUE ZERO.
       77  WS-AVGE     PIC 9(06)V999 COMP-3.
       77  WS-PROG          PIC X(02).
       77  WS-OPTION        PIC X(01).
       77  TODAY-DDMMYY    PIC 9(06) COMP-5.
       77  WS-USUB    PIC 9(04) COMP-5.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Used by calling program (WORKING-STORAGE) and called program ³
      *    ³ (LINKAGE SECTION) for Main screen layout and headings.       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       01  CRT-HEADINGS.
      *
      *    TYPE:
      *         4 = Clear line 46 and line 50
      *         C = Clear comment line (50)
      *         E = Clear lines (any line or lines between 5 and 46)
      *         H = Change heading
      *         S = Clear the screen and display headings
      *
           03  CRT-TYPE            PIC  X(01).
      *
      *    The following two fields are used for clearing a portion of
      *    the screen. (Start line and end line)
      *
           03  CRT-START           PIC  9(02) VALUE 5.
           03  CRT-END             PIC  9(02) VALUE 46.
      *
      *    Calling Program
      *
           03  CRT-PROGRAM         PIC  X(15) VALUE "STKCON".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "STOCK/INVENTORY -CONVERT FILE UTILITY".
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

       01  WS-PARID.
           03  WS-SYS-ID           PIC  X(03).

       01  W12-DATE.
           03  W12-TODAY           PIC  9(06).
           03  W12-DATE-DMY                      REDEFINES W12-TODAY.
               05  W12-DAY         PIC  9(02).
               05  W12-MONTH       PIC  9(02).
               05  W12-YEAR        PIC  9(02).
           03  W12-T-YMD           PIC  9(06).
           03  W12-DATE-YMD                      REDEFINES W12-T-YMD.
               05  W12-YY          PIC  9(02).
               05  W12-MM          PIC  9(02).
               05  W12-DD          PIC  9(02).

       COPY "FUNCTION.WS".

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
      *   *****    *****   ******   ******  ******  **    **
      *  **   **  **   **  **   **  **      **      ***   **
      *  **       **       **  **   **      **      ****  **
      *   *****   **       *****    *****   *****   ** ** **
      *       **  **       **  **   **      **      **  ****
      *  **   **  **   **  **   **  **      **      **   ***
      *   *****    *****   **   **  ******  ******  **    **
      *
       SCREEN SECTION.

       01  S00.
           03  LINE  2 COLUMN 28 FOREGROUND-COLOR Grey HIGHLIGHT
               VALUE "STOCK CONVERTION PROGRAM".
    03  LINE  3 COLUMN  2
         FOREGROUND-COLOR Brown HIGHLIGHT VALUE "STOCK FILE".
           03  LINE  4 COLUMN  2 VALUE "Reading :".
           03  LINE  6 COLUMN  2 VALUE "Writing :".
    03  LINE  8 COLUMN  2
               FOREGROUND-COLOR Brown HIGHLIGHT VALUE "PRODUCTION FILE".
          03  LINE  9 COLUMN  2 VALUE "Reading :".
          03  LINE 11 COLUMN  2 VALUE "Writing :".
    03  LINE 13 COLUMN  2
               FOREGROUND-COLOR Brown HIGHLIGHT VALUE "MEMO FILE".
          03  LINE 14 COLUMN  2 VALUE "Reading :".
          03  LINE 16 COLUMN  2 VALUE "Writing :".

       01  S05.
          03  LINE 18 COLUMN 12 FOREGROUND-COLOR Cyan HIGHLIGHT
               VALUE "Convertion unsuccessfull !".
          03  LINE 19 COLUMN 12 FOREGROUND-COLOR Cyan HIGHLIGHT
                       VALUE "Reload files from last backup and rekey all updates and transactions".
          03  LINE 20 COLUMN 12 FOREGROUND-COLOR Cyan HIGHLIGHT
                       VALUE "since creating the last backup disk/s".
      *
      *      ******   ******    *****    *****   ******  ******   **   **  ******    ****** 
      *      **   **  **   **  **   **  **   **  **      **   **  **   **  **   **   **
      *      **   **  **  **   **   **  **       **      **   **  **   **  **  **    **
      *      ******   *****    **   **  **       *****   **   **  **   **  *****     *****
      *      **       **  **   **   **  **       **      **   **  **   **  **  **    **
      *      **       **   **  **   **  **   **  **      **   **  **   **  **   **   **
      *      **       **   **   *****    *****   ******  ******    *****   **   **   ******
      *
       PROCEDURE DIVISION USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY DATA-FILE-IDS.
       AA000        SECTION.
       AA00.
            PERFORM ZA000.
           IF WS-ERROR = 9
        GO TO AA50.
             DISPLAY S00.
       AA05.
            READ STKOLD
               AT END
              GO TO AA10.
      DISPLAY STO-CODE AT 0412 WITH FOREGROUND-COLOR Cyan HIGHLIGHT
       " " STO-DESC WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
             PERFORM AW000.
             GO TO AA05.
       AA10.
            READ SPAOLD
               AT END
              GO TO AA15.
      DISPLAY PRO-ITEM AT 0912 WITH FOREGROUND-COLOR Cyan HIGHLIGHT
       " " PRO-COMMENT WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
            PERFORM AX000.
      GO TO AA10.

       AA15.
            READ STMOLD
               AT END
              GO TO AA20.
      DISPLAY SMO-CODE AT 1412 WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
            PERFORM AY000.
      GO TO AA15.

       AA20.
      MOVE "6.62"  TO STK-VER.
      PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
            CLOSE STOCK
     STKOLD
     SPARTS
     SPAOLD
     STKMEM
     STMOLD.
            CALL "CBL_DELETE_FILE" USING W02-STKOLD
              RETURNING WS-STATUS.
            CALL "CBL_DELETE_FILE" USING W02-SPAOLD
              RETURNING WS-STATUS.
            CALL "CBL_DELETE_FILE" USING W02-STMOLD
              RETURNING WS-STATUS.

       AA45.
            EXIT PROGRAM.
       AA50.
             DISPLAY S05.
             STOP RUN.

       COPY "LOCKED.RC2".

      /
      *    ****    READ FILES ROUTINES
      *
       AC000-READ              SECTION.

       COPY "CONTROL.RD".

       COPY "PARAM.RD".

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
       AD000-WRITE             SECTION.

       COPY "CONTROL.WR".

       COPY "STOCK.WR".

       COPY "SPARTS.WR".

       COPY "STKMEM.WR".

      /
      *    THIS ROUTINE WRITES THE DETAILS TO THE NEW FILE.
      *
       AW000           SECTION.
       AW01.
   IF NOT (STK-VER = "6.5")
             MOVE STO-O-AVGE  TO WS-AVGE
       MOVE WS-AVGE  TO STO-AVGE.
            MOVE STO-CODE  TO STK-CODE.
            MOVE STO-ACODE  TO STK-ACODE.
            MOVE STO-BIN  TO STK-BIN.
      MOVE SPACES  TO STK-ITM.
            MOVE STO-LEDG  TO STK-LEDG.
            MOVE STO-DESC  TO STK-DESC.
      MOVE SPACES  TO STK-DESC2.
            MOVE STO-QUANT  TO STK-QUANT.
            MOVE STO-LEVEL  TO STK-LEVEL.
            MOVE STO-COST  TO STK-COST.
            MOVE STO-SELL  TO STK-SELL.
            MOVE STO-CASH  TO STK-CASH.
            MOVE STO-WSALE  TO STK-WSALE.
            MOVE STO-WSALE  TO STK-SELL4.
            MOVE STO-AVGE  TO STK-AVGE.
            MOVE STO-ORD  TO STK-ORD.
            MOVE STO-LTIME  TO STK-LTIME.
            MOVE STO-MTD  TO STK-MTD.
            MOVE STO-MTDC  TO STK-MTDC.
            MOVE STO-MTDV  TO STK-MTDV.
            MOVE STO-YTD  TO STK-YTD.
            MOVE STO-YTDC  TO STK-YTDC.
            MOVE STO-YTDV  TO STK-YTDV.
            MOVE STO-MTH1  TO STK-MTH1.
            MOVE STO-MTH2  TO STK-MTH2.
            MOVE STO-MTH3  TO STK-MTH3.
            MOVE STO-MAVG  TO STK-MAVG.
            MOVE STO-MARKUP  TO STK-MARKUP.
            MOVE STO-CMARKUP  TO STK-CMARKUP.
            MOVE STO-WMARKUP  TO STK-WMARKUP.
            MOVE STO-WMARKUP  TO STK-MARKUP4.
            MOVE STO-ACT  TO STK-ACT.
            MOVE STO-PUR  TO STK-PUR.
            MOVE STO-DTE  TO STK-DTE.
            MOVE STO-DISC  TO STK-DISC.
            MOVE STO-IND  TO STK-IND.
            MOVE STO-SUPP  TO STK-SUPP.
      MOVE SPACES  TO STK-PSUPP.
            MOVE STO-PRC  TO STK-PRC.
            MOVE STO-GUAR  TO STK-GUAR.
            MOVE STO-TAX  TO STK-TAX.
            MOVE STO-XREF  TO STK-XREF.
      MOVE "Y"   TO STK-PLIST.
      MOVE STO-PRC  TO STK-PER.
      MOVE ZERO   TO STK-WK1  STK-WK2  STK-WK3
        STK-WK4  STK-QJOB STK-QPIC
        STK-PRO-START STK-PRO-END
        STK-PRO-PRICE.
             DISPLAY STK-CODE AT 0612 WITH FOREGROUND-COLOR Brown HIGHLIGHT
                     " " STK-DESC WITH FOREGROUND-COLOR Brown HIGHLIGHT.
            PERFORM WRITE-STOCK THRU WRITE-STOCK-EXIT.

       AW999.
             EXIT.
      /
      *    THIS ROUTINE WRITES THE DETAILS TO THE NEW FILE.
      *
       AX000        SECTION.
       AX01.
            MOVE PRO-ITEM  TO PRT-ITEM.
            MOVE SPACES  TO PRT-ITM.
            MOVE PRO-COMMENT  TO PRT-COMMENT.
      MOVE PRO-TYPE  TO PRT-TYPE.
      MOVE 1   TO WS-S1.

       AX05.
            MOVE PRO-CODE (WS-S1)
     TO PRT-CODE (WS-S1).
      MOVE SPACES  TO PRT-PITM (WS-S1).
            MOVE PRO-QUANT (WS-S1)
     TO PRT-QUANT (WS-S1).
          IF WS-S1 < 30
              ADD 1   TO WS-S1
        GO TO AX05.
      DISPLAY PRT-ITEM AT 1112 WITH FOREGROUND-COLOR Brown HIGHLIGHT
       " " PRT-COMMENT WITH FOREGROUND-COLOR Brown HIGHLIGHT.
            PERFORM WRITE-SPARTS THRU WRITE-SPARTS-EXIT.

       AX999.
             EXIT.
      /
      *    THIS ROUTINE WRITES THE DETAILS TO THE NEW FILE.
      *
       AY000        SECTION.
       AY01.
            MOVE SMO-CODE  TO SME-CODE.
            MOVE SPACES  TO SME-ITM.
            MOVE SMO-SEQ  TO SME-SEQ.
      MOVE SMO-DATA  TO SME-DATA.
      DISPLAY SME-CODE AT 1612 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
            PERFORM WRITE-STKMEM THRU WRITE-STKMEM-EXIT.

       AY999.
             EXIT.
      /
       ZA000           SECTION.
       ZA00.
             MOVE LS-PARID           TO WS-PARID.
             MOVE LS-TODAY-DDMMYY    TO TODAY-DDMMYY W12-TODAY.
             MOVE LS-TODAY-YYMMDD    TO W12-T-YMD.
             MOVE LS-USUB            TO WS-USUB.
             MOVE LS-COMPANY         TO CRT-COMPANY.
             MOVE LS-TERMINAL        TO CRT-TERMINAL.
             PERFORM HEADING-AND-CLEAR-SCREEN.      
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

             COPY "FUNCTION.PRO".

       ZA00A-CONTINUE.
       ZA01B.
             CALL "CBL_RENAME_FILE" USING W02-STOCKF
             W02-STKOLD
              RETURNING WS-STATUS.
            OPEN INPUT STKOLD.
          IF WS-STAT1 = "9"
               IF FLE-LOCKED
                   GO TO ZA200.
           IF WS-STATUS NOT = "00"
               MOVE 72 TO WS-F-ERROR
               PERFORM OPEN-ERROR.
             OPEN OUTPUT STOCK.
           IF WS-STAT1 = "9"
               IF FLE-LOCKED
                   GO TO ZA200.
           IF WS-STATUS NOT = "00"
               MOVE 22 TO WS-F-ERROR
               PERFORM OPEN-ERROR.
            CALL "CBL_RENAME_FILE" USING W02-SPARTS
             W02-SPAOLD
              RETURNING WS-STATUS.
            OPEN INPUT SPAOLD.
           IF WS-STAT1 = "9"
               IF FLE-LOCKED
                   GO TO ZA200.
           IF WS-STATUS NOT = "00"
               MOVE 71 TO WS-F-ERROR
               PERFORM OPEN-ERROR.
             OPEN OUTPUT SPARTS.
           IF WS-STAT1 = "9"
               IF FLE-LOCKED
                   GO TO ZA200.
           IF WS-STATUS NOT = "00"
               MOVE 21 TO WS-F-ERROR
               PERFORM OPEN-ERROR.
            CALL "CBL_RENAME_FILE" USING W02-STKMEM
             W02-STMOLD
              RETURNING WS-STATUS.
            OPEN INPUT STMOLD.
           IF WS-STAT1 = "9"
               IF FLE-LOCKED
                   GO TO ZA200.
           IF WS-STATUS NOT = "00"
               MOVE 75 TO WS-F-ERROR
               PERFORM OPEN-ERROR.
             OPEN OUTPUT STKMEM.
           IF WS-STAT1 = "9"
               IF FLE-LOCKED
                   GO TO ZA200.
           IF WS-STATUS NOT = "00"
               MOVE 25 TO WS-F-ERROR
               PERFORM OPEN-ERROR.
            MOVE 3   TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
      GO TO ZA999-EXIT.
       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
             ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR Grey HIGHLIGHT.
             GO TO ZA205.
       ZA50.
           IF WS-F-ERROR = 22
        MOVE "STOCK file" TO WS-FILE.
            DISPLAY "Rebuild " AT 0812
                      WS-FILE WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             DISPLAY "Press any key to continue" AT 1012.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
       ZA51.
             EXIT PROGRAM.
       ZA200.
             DISPLAY "Files locked - Try later" AT 2312
                      WITH FOREGROUND-COLOR Brown HIGHLIGHT
                     " " WS-STATUS WITH FOREGROUND-COLOR Grey HIGHLIGHT.
             DISPLAY "Press any key" AT 5012
              WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT.
            ACCEPT WS-OPTION AT 5026
      WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Grey HIGHLIGHT AUTO.
       ZA205.
             EXIT PROGRAM.
       ZA999-EXIT.
             EXIT.
      /
       ZB000-ERROR           SECTION.

       OPEN-ERROR.
             PERFORM ERASE-SCREEN.
             DISPLAY "Open error" AT 0812 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             PERFORM DISPLAY-FILE-NAME.

       READ-ERROR.
             PERFORM ERASE-SCREEN.
             DISPLAY "Read error" AT 0812 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             PERFORM DISPLAY-FILE-NAME.

       WRITE-ERROR.
             PERFORM ERASE-SCREEN.
             DISPLAY "Write error" AT 0812 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             PERFORM DISPLAY-FILE-NAME.

       DISPLAY-FILE-NAME.
          IF WS-F-ERROR = 2
               MOVE W02-NETWORK  TO WS-FILE
              MOVE WS-NETKEY  TO WS-KEY
           ELSE
          IF WS-F-ERROR = 15
               MOVE WS-PARID     TO WS-FILE
               MOVE WS-PARKEY    TO WS-KEY
          ELSE
           IF WS-F-ERROR = 21
              MOVE W02-SPARTS  TO WS-FILE
              MOVE ZERO  TO WS-KEY
          ELSE
          IF WS-F-ERROR = 22
               MOVE W02-STOCKF   TO WS-FILE
               MOVE ZERO         TO WS-KEY
        MOVE STK-CODE  TO WS-KEYX
           ELSE
           IF WS-F-ERROR = 25
              MOVE W02-STKMEM  TO WS-FILE
              MOVE ZERO  TO WS-KEY
          ELSE
           IF WS-F-ERROR = 71
              MOVE W02-SPAOLD  TO WS-FILE
              MOVE ZERO  TO WS-KEY
           ELSE
           IF WS-F-ERROR = 72
               MOVE W02-STKOLD   TO WS-FILE
              MOVE ZERO  TO WS-KEY
           ELSE
           IF WS-F-ERROR = 75
              MOVE W02-STMOLD  TO WS-FILE
              MOVE ZERO  TO WS-KEY.
          IF WS-STATUS = "10"
               MOVE "End of FILE"
                                 TO WS-STAT-MESSAGE
           ELSE
           IF WS-STATUS = "22"
               MOVE "Duplicate record number"
                                 TO WS-STAT-MESSAGE
           ELSE
           IF WS-STATUS = "23"
               MOVE "Invalid record number"
                                 TO WS-STAT-MESSAGE
           ELSE
           IF WS-STATUS = "24"
               MOVE "DISK full"  TO WS-STAT-MESSAGE
           ELSE
           IF WS-STATUS = "30"
               MOVE "DISK error" TO WS-STAT-MESSAGE
           ELSE
           IF WS-STATUS = "94"
               MOVE "FILE locked" TO WS-STAT-MESSAGE.
             DISPLAY "File - " AT 1012 WS-FILE WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
             DISPLAY "Status " AT 1212
                      WS-STATUS WITH FOREGROUND-COLOR Cyan HIGHLIGHT
                     ": " WS-STAT-MESSAGE WITH FOREGROUND-COLOR Brown HIGHLIGHT.
           IF WS-STATUS NOT = "94"
               DISPLAY "Key    " AT 1412
                        WS-KEY WITH FOREGROUND-COLOR Cyan HIGHLIGHT
               DISPLAY "Reverse backup or contact program Support"
                        AT 1612.
               DISPLAY "Please make a note of these details" AT 1812.
             STOP RUN.
