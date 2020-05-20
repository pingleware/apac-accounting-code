      $set LINKCOUNT"512" GNT"GLP004.GNT"
      ******************************************************************
      *                                                                *
      *    ******   **        *******     ****      ****        **     *
      *   **    **  **        **    **   **  **    **  **      ***     *
      *   **        **        **    **  **    **  **    **    * **     *
      *   **        **        *******   **    **  **    **   *  **     *
      *   **   ***  **        **        **    **  **    **  *******    *
      *   **    **  **        **         **  **    **  **       **     *
      *    ******   ********  **          ****      ****       ****    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     G / L E D G E R   A C C U M U L A T E   G R O U P          *
      *                                                                *
      *       Version 9.04.02 - June 2017                              *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.        GLP004.
       AUTHOR.                 J.W. LEMMON (APAC).
       DATE-WRITTEN.           JUNE 1983.

                COPYRIGHT NOTICE: COPYRIGHT (C) 1983 - 2017
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

       COPY "CONTROL.SL".

       COPY "PARAM.SL".

       COPY "GACCNT.SL".

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

       COPY "GACCNT.FDE".

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
       77  WS-PARKEY               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-LTRKEY               PIC  9(06)    COMP-5 VALUE 0.
       77  WS-ENQPOS               PIC  9(06)    COMP-5.
       77  WS-ENQEND               PIC  9(06)    COMP-5.
       77  WS-NETKEY               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-KSTORE               PIC  9(06)    COMP-5.
       77  WS-SKSTORE              PIC  9(06)    COMP-5.
       77  WS-INKEY                PIC  9(06)    COMP-5.
       77  WS-COKEY                PIC  9(06)    COMP-5.
       77  WS-GLJSTR               PIC  9(04)    COMP-5.
       77  WS-GLJKEY               PIC  9(06)    COMP-5.
       77  WS-RECKEY               PIC  9(04)    COMP-5.
       77  WS-RECOVER              PIC  9(04)    COMP-5.
       77  WS-TRANS                PIC  9(04)    COMP-5 VALUE 1.
       77  WS-ISUB                 PIC  9(04)    COMP-5.
       77  WS-CHEQUE               PIC  9(04)    COMP-5.
       77  WS-SUB                  PIC  9(04)    COMP-5.
       77  WS-S                    PIC  9(04)    COMP-5.
       77  WS-S1                   PIC  9(04)    COMP-5.
       77  WS-S2                   PIC  9(04)    COMP-5.
       77  WS-S3                   PIC  9(04)    COMP-5.
       77  WS-S4                   PIC  9(04)    COMP-5.
       77  WS-S6                   PIC  9(04)    COMP-5.
       77  WS-S9                   PIC  9(04)    COMP-5.
       77  WS-Y-END                PIC  9(02)           VALUE ZERO.
       77  WS-L-END                PIC  9(04)           VALUE ZERO.
       77  WS-OPEN                 PIC  9(02)           VALUE ZERO.
       77  WS-MS1                  PIC  9(04)    COMP-5.
       77  WS-MS2                  PIC  9(04)    COMP-5.
       77  WS-VALUE                PIC S9(09)V99 COMP-3.
       77  WS-EXS                  PIC  9(01).
       77  WS-CONREC               PIC  9(01)           VALUE ZERO.
       77  WS-OPTION               PIC  X(01).
       77  WS-SKIP                 PIC  X(01)           VALUE "N".
       77  WS-ERROR                PIC  9(01)           VALUE ZERO.
       77  WS-NUM                  PIC  Z9.
       77  WS-TNEXT                PIC  9(04)    COMP-5.
       77  WS-PERIOD               PIC  9(02)           VALUE ZERO.
       77  TODAY-DDMMYY            PIC  9(08)    COMP-5.
       77  WS-USUB                 PIC  9(04)    COMP-5.

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
      *         S =  Clear the screen and display headings
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "GLP004".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "GENERAL LEDGER -ACCUMULATE GROUP TOTALS".
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
          03  WS-SYS-ID       PIC  X(03).

       COPY "W05.GL".

       01  W10-H2.
           03  FILLER      PIC  X(27).
           03  W10-HEAD    PIC  X(13).

       COPY "W15.GL".

       01  W20-TOTALS.
           03  W20-DEBIT   PIC S9(09)V99 COMP-3.
           03  W20-CREDIT  PIC S9(09)V99 COMP-3.
           03  W20-RESULT  PIC S9(09)V99.
           03  W20-RES1    REDEFINES W20-RESULT.
               05  W20-WHOLE
                           PIC  9(09).
               05  W20-DEC PIC S9(02).
           03  W20-TOTAL   PIC S9(09)V99 COMP-3.
           03  W20-STOTAL  PIC S9(09)V99 COMP-3.
           03  W20-BTOTAL  PIC S9(09)    COMP-3.
           03  W20-INTOT   PIC S9(09)V99 COMP-3.
           03  W20-COTOT   PIC S9(09)V99 COMP-3.
           03  W20-GTOT.
               05  W20-GRPTOT
                           PIC S9(09)V99 COMP-3 OCCURS 12.
           03  W20-STOT.
               05  W20-SUBTOT
                           PIC S9(09)V99 COMP-3 OCCURS 12.
       01  W20-INPUT.
           03  W20-ACCOUNT PIC  9(06).
           03  W20-ACNO1   REDEFINES W20-ACCOUNT.
               05  W20-ACNO
                           PIC 9(04).
               05  W20-SER PIC  9(02).
           03  W20-ACNO2   REDEFINES W20-ACCOUNT.
               05  W20-GR  PIC  9(02).
               05  W20-SGRP
                           PIC 9(02).
               05  W20-ASER
                           PIC 9(02).
           03  W20-ACNO3   REDEFINES W20-ACCOUNT.
               05  W20-GROUP
                           PIC 9(02).
               05  W20-REST
                           PIC 9(04).

       COPY "W20.WS".

       01  W25-CALCS.
          03  W25-RESULT  PIC 9(05)V99.
           03  W25-RESULT1 REDEFINES W25-RESULT.
               05  W25-WHOLE   PIC 9(05).
               05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
        05  W25-KEY PIC 9(04).
        05  W25-SUB PIC 9(01).
        05  FILLER  PIC 9(02).

       01  W25-EDIT.
           03  W25-7.
               05  W25-1   PIC  ZZZZZZZZ9-.
           03  W25-S7V2.
               05  W25-V1  PIC  Z(07)9.99CR.
          03  W25-S8V2.
              05  W25-VG1 PIC Z(07)9.99CR.
          03  W25-DATE.
              05  W25-DTE PIC Z9/99/9999.
           03  W25-REF.
               05  W25-REFER
                           PIC Z(07).
           03  W25-RECKEY  PIC  Z(04)9.

       COPY "W30.GL".
       
       COPY "W40.WS".

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       COPY "W50-70.GL".

       COPY "W70.WS".

       01  W90-COMP.
           03  W90-CNAME   PIC  X(40).

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
           IF LS0-GLLEV < 2
      *
      *      Insufficient security level
      *
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
               GO TO AA49.
             PERFORM ZA000.
             PERFORM LA000.

       AA49.
             EXIT PROGRAM.

       COPY "FUNCTION.SCR".

       COPY "ERROR.SCR".

       COPY "LOCKED.REC".

       COPY "CLEAR.CRT".

      *
      *    ****    R E A D   F I L E S   R O U T I N E S
      *
       AB000           SECTION.

       COPY "CONTROL.RD".

       COPY "PARAM.RD".

       COPY "GACCNT.RD".

      *                                                                 
      *     R E W R I T E   &   W R I T E   F I L E S   R O U T I N E S 
      *                                                                 
       AC000           SECTION.

       COPY "CONTROL.WR".

       COPY "GACCNT.WR".

      /
       LA000      SECTION 5.
       LA00.
             PERFORM ERASE-SCREEN.
            DISPLAY "Accumulate Group & Sub-Group Totals" AT 0422
              WITH FOREGROUND-COLOR Grey HIGHLIGHT.
            DISPLAY "Are you sure (Y/N)?" AT 0622
              WITH FOREGROUND-COLOR Brown HIGHLIGHT.
            ACCEPT WS-OPTION AT 0642
      WITH FOREGROUND-COLOR Grey HIGHLIGHT AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
          IF WS-OPTION = "N"
               GO TO LA999.
             MOVE W05-MM         TO WS-PERIOD.
      DISPLAY "Month to be accumulated :" AT 0822.
       LA02.
      ACCEPT WS-PERIOD AT 0848
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta UPDATE AUTO.
      PERFORM CLEAR-L50.
           IF WS-PERIOD < 01 OR > 12
        MOVE "Invalid month"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
        GO TO LA02.
            DISPLAY "Account No:" AT 1422.
             MOVE 4              TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
             MOVE ZERO TO W15-ACCOUNT W20-ACCOUNT WS-KSTORE
                          WS-SKSTORE W20-TOTAL W20-STOTAL W20-BTOTAL
                          WS-INKEY WS-COKEY W20-INTOT W20-COTOT.
      MOVE 100000  TO G-AC.
      PERFORM START-AT-LEDG-AC THRU READ-GACCNT-EXIT.
    IF WS-F-ERROR = 8
              GO TO LA999.
       LA05.
      PERFORM READ-GACCNT-NEXT THRU READ-GACCNT-EXIT.
          IF WS-F-ERROR = 8
        GO TO LA100.
            MOVE G-AC   TO W15-ACCOUNT W20-ACCOUNT.
            DISPLAY G-AC AT 1434
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
           IF W15-REST NOT = ZERO
               GO TO LA90.
           IF G-AC NOT = 100000
               GO TO LA999.
            MOVE G-AC   TO WS-KSTORE WS-INKEY.

       LA10.
      PERFORM READ-GACCNT-NEXT THRU READ-GACCNT-EXIT.
          IF WS-F-ERROR = 8
        GO TO LA100.
            MOVE G-AC   TO W20-ACCOUNT.
            DISPLAY G-AC AT 1434
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
          IF G-AC = 200000
              MOVE G-AC  TO WS-COKEY.
           IF W20-ACCOUNT NOT = W70-UNPROF
               GO TO LA15.
            MOVE 4   TO WS-NETKEY.
            PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
      ADD G-PL-MTH (WS-PERIOD)
     TO W20-STOTAL.
            PERFORM REWRITE-CONTROL-UNLOCK THRU WRITE-GACCNT-EXIT.
            GO TO LA30.

       LA15.
           IF W20-GR = W15-GR
               GO TO LA20.
           IF W20-REST NOT = ZERO
               GO TO LA90.
           IF W15-SGRP = ZERO
              MOVE W20-STOTAL  TO W20-TOTAL
               GO TO LA70.
             PERFORM LA50 THRU LA60.
             GO TO LA70.

       LA20.
           IF W20-SGRP = ZERO
               GO TO LA30.
           IF W20-SGRP = W15-SGRP
               GO TO LA30.
           IF W20-ASER NOT = ZERO
               GO TO LA90.
           IF W15-SGRP NOT = ZERO
               GO TO LA50.
           IF W20-STOTAL NOT = ZERO
               GO TO LA90.
             GO TO LA60.

       LA30.
            ADD G-BVAL (WS-PERIOD)
     TO W20-BTOTAL.
           IF W05-MM NOT = WS-PERIOD
        ADD G-VAL (WS-PERIOD)
                                 TO W20-STOTAL
           ELSE
               ADD G-BAL         TO W20-STOTAL.
             GO TO LA10.

       LA50.
            MOVE WS-SKSTORE  TO G-AC.
             PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
            MOVE W20-BTOTAL  TO G-BVAL (WS-PERIOD).
      MOVE W20-STOTAL  TO G-VAL (WS-PERIOD).
    IF W05-MM = WS-PERIOD
              MOVE W20-STOTAL  TO G-BAL.
             PERFORM REWRITE-GACCNT THRU WRITE-GACCNT-EXIT.
       LA60.
            MOVE W20-ACCOUNT  TO WS-SKSTORE.
            MOVE W20-SGRP  TO W15-SGRP.
            ADD W20-STOTAL  TO W20-TOTAL.
            MOVE ZERO   TO W20-STOTAL W20-BTOTAL.
       LA65.
            MOVE W20-ACCOUNT  TO G-AC.
      PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
            GO TO LA10.
       LA70.
            MOVE WS-KSTORE  TO G-AC.
             PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
      MOVE W20-TOTAL  TO G-VAL (WS-PERIOD).
    IF W05-MM = WS-PERIOD
              MOVE W20-TOTAL  TO G-BAL.
             PERFORM REWRITE-GACCNT THRU WRITE-GACCNT-EXIT.
           IF G-GRP = 1
              ADD W20-TOTAL  TO W20-INTOT.
           IF G-GRP = 2
              ADD W20-TOTAL  TO W20-COTOT.
       LA80.
            MOVE W20-ACCOUNT  TO WS-KSTORE G-AC.
            MOVE W20-ACNO  TO W15-ACNO.
            MOVE ZERO   TO W20-TOTAL.
      PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
       LA85.
            GO TO LA10.
       LA90.
             PERFORM ERASE-SCREEN.
            DISPLAY "Missing Group or Sub-Group heading" AT 0612
              WITH FOREGROUND-COLOR Brown HIGHLIGHT.
            DISPLAY "Last account number at :" AT 0812
              WITH FOREGROUND-COLOR Brown HIGHLIGHT
              W20-ACCOUNT WITH FOREGROUND-COLOR Grey HIGHLIGHT.
      DISPLAY "Press any key to exit and rectify" AT 1012
        WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
            ACCEPT WS-OPTION AT 1046
      WITH FOREGROUND-COLOR Grey HIGHLIGHT.
             GO TO LA999.
       LA100.
           IF W15-SGRP NOT = ZERO
               PERFORM LA50.
            ADD W20-STOTAL  TO W20-TOTAL.
             PERFORM LA70.
            MOVE WS-INKEY  TO G-AC.
             PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
           IF W05-MM NOT = WS-PERIOD
              MOVE W20-INTOT  TO G-BVAL (WS-PERIOD)
           ELSE
              MOVE W20-INTOT  TO G-BVAL (W05-MM).
             PERFORM REWRITE-GACCNT THRU WRITE-GACCNT-EXIT.
           IF WS-COKEY > ZERO
              MOVE WS-COKEY  TO G-AC
               PERFORM READ-GACCNT THRU READ-GACCNT-EXIT
               IF W05-MM NOT = WS-PERIOD
                   MOVE W20-COTOT
                                 TO G-BVAL (WS-PERIOD)
               ELSE
                   MOVE W20-COTOT
                                 TO G-BVAL (W05-MM)
               END-IF
               PERFORM REWRITE-GACCNT THRU WRITE-GACCNT-EXIT.
       LA999.
             EXIT.
      *    *************************************************************
      *                I N I T I A L I S E   P R O G R A M
      *    *************************************************************
       ZA000      SECTION 9.
       ZA00.
             MOVE LS-PARID           TO WS-PARID.
             MOVE LS-TODAY-DDMMYY    TO TODAY-DDMMYY W05-TODAY.
             MOVE LS-TODAY-YYMMDD    TO W05-TYMD.
             MOVE LS-USUB            TO WS-USUB.
             MOVE LS-COMPANY         TO CRT-COMPANY W90-CNAME.
             MOVE LS-TERMINAL        TO CRT-TERMINAL.
             PERFORM HEADING-AND-CLEAR-SCREEN.      
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "FUNCTION.PRO".

       ZA02.
             MOVE 4                  TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
           IF G-YEAR-END = ZERO
               MOVE 2                TO G-YEAR-END
               PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
      *
      *    ****    S E T   U P   M O N T H S   ( Y E A R   E N D)
      *
             MOVE 1                  TO WS-S1.
             MOVE G-YEAR-END         TO WS-S2 WS-Y-END.
             MOVE G-LAST-YE          TO WS-L-END.
             MOVE G-OPEN             TO WS-OPEN.
             MOVE 6                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-UNPROF         TO W70-UNPROF.
             MOVE WS-Y-END           TO WS-S1.
             MOVE W05-CY             TO WS-S2.
           IF W05-MM < WS-Y-END
               SUBTRACT 1            FROM WS-S2.

       ZA20.
            ADD 1   TO WS-S1.
           IF WS-S1 > 12
              MOVE 1   TO WS-S1
              ADD 1   TO WS-S2.
            MOVE WS-S2   TO W40-YEAR (WS-S1).
           IF WS-S1 NOT = WS-Y-END
               GO TO ZA20.
      DISPLAY SPACES AT 5025
       WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.

       ZA999.
             EXIT.
      *
      *     I / O   E R R O R   M E S S A G E S
      *
       ZB000      SECTION 9.

       COPY "ERRORS.PRO".

       DISPLAY-FILE-NAME.
             MOVE ZERO               TO WS-KEY.
             EVALUATE WS-F-ERROR
               WHEN 2          MOVE W02-NETWORK  TO WS-FILE
                               MOVE WS-NETKEY    TO WS-KEY
               WHEN 8          MOVE W02-GACCNT   TO WS-FILE
                               MOVE G-AC         TO WS-KEYX
               WHEN 15         MOVE WS-PARID     TO WS-FILE
                               MOVE WS-PARKEY    TO WS-KEY
               WHEN OTHER      MOVE "ERROR"      TO WS-FILE
                               MOVE "GLP004"     TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
