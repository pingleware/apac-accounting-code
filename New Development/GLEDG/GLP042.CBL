      $set LINKCOUNT"384" GNT"GLP042.GNT"
      ******************************************************************
      *                                                                *
      *    ******   **       *******   ****       **     ******    *
      *   ** **  **       **    **  **  **      ***    **   **   *
      *   **     **       **    ** **    **    * **  **    *
      *   **     **       ******* **    **   *  **       **      *
      *   **   ***  **       ** **    **  *******    **        *
      *   ** **  **       **  **  **       **    **        *
      *    ******   ********  **   ****      ****   ********   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *  G / L E D G E R   D E L E T E   R E C U R I N G   E N T R Y   *
      *                                                                *
      *       Version 9.04.02 - June 2017                              *
      *               *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.        GLP042.
       AUTHOR.                 J.W. LEMMON (APAC).
       DATE-WRITTEN.           NOVEMBER 1984.

                COPYRIGHT NOTICE: COPYRIGHT (C) 1984 - 2017
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

       COPY "GACCNT.SL".

          SELECT GLINST  ASSIGN DISK
                          LOCK EXCLUSIVE
                          STATUS WS-STATUS
                          ACCESS RANDOM
                          ORGANIZATION RELATIVE
                          RELATIVE WS-GLINSTKEY.

       COPY "PARAM.SL".

       COPY "SHARED.SL".

       DATA DIVISION.
       FILE SECTION.

       COPY "CONTROL.FDE".

       COPY "GACCNT.FDE".

       COPY "GLINST.FD".

       COPY "PARAM.FDE".

       COPY "SHARED.FDE".

      /
       WORKING-STORAGE SECTION.
       77  WS-CHECK                  PIC  X(18)    VALUE "aeWlimemnomLalismJ".
       77  WS-SUB    PIC 9(04)  COMP-5.
       77  WS-PARKEY       PIC 9(05)     COMP-0 VALUE 1.
       77  WS-NETKEY    PIC 9(05)  COMP-0 VALUE 1.
       77  WS-GLINSTKEY    PIC 9(05)  COMP-0.
       77  WS-SHARED    PIC 9(04)  COMP-5 VALUE 1.
       77  WS-KSTORE    PIC 9(05)  COMP-0.
       77  WS-ISUB         PIC 9(05)     COMP-0.
       77  WS-CHEQUE       PIC 9(05)     COMP-0.
       77  WS-PAGE         PIC 9(05)     COMP-0.
       77  WS-MONTH        PIC 9(02) VALUE ZERO.
       77  WS-LC           PIC 9(02) VALUE ZERO.
       77  WS-S1           PIC 9(05)     COMP-0.
       77  WS-S2           PIC 9(05)     COMP-0.
       77  WS-S3           PIC 9(05)     COMP-0.
       77  WS-S4           PIC 9(05)     COMP-0.
       77  WS-S5           PIC 9(05)     COMP-0.
       77  WS-S6           PIC 9(05)     COMP-0.
       77  WS-S7           PIC 9(05)     COMP-0.
       77  WS-S8           PIC 9(05)     COMP-0.
       77  WS-S9           PIC 9(05)     COMP-0.
       77  WS-S10          PIC 9(05)     COMP-0.
       77  WS-Y-END        PIC  9(02) VALUE ZERO.
       77  WS-L-END    PIC 9(04) VALUE ZERO.
       77  WS-OPEN         PIC  9(02) VALUE ZERO.
       77  WS-MS1          PIC  9(04)    COMP-5.
       77  WS-MS2          PIC  9(04)    COMP-5.
       77  WS-VALUE        PIC S9(09)V99 COMP-3.
       77  WS-EXS          PIC 9(01) VALUE ZERO.
       77  WS-INDG         PIC 9(01) VALUE ZERO.
       77  WS-NUM          PIC Z9.
       77  WS-OPTION       PIC X(01).
       77  WS-SKIP         PIC X(01) VALUE "Y".
       77  WS-ERROR        PIC 9(02) VALUE ZERO.
       77  WS-NORM         PIC  X(04).
       77  WS-COND         PIC  X(04).
       77  WS-EXPP         PIC  X(04).
       77  WS-ECAN         PIC  X(04).
       77  WS-8LPI       PIC  X(04).
       77  WS-6LPI       PIC  X(04).
       77  WS-10CPI        PIC  X(04).
       77  WS-12CPI        PIC  X(04).
       77  WS-17CPI        PIC  X(04).
       77  WS-DBLP         PIC  X(04).
       77  WS-DCAN         PIC  X(04).
       77  WS-BATCH        PIC 9(05)     COMP-3.
       77  WS-TNEXT        PIC 9(05)     COMP-0.
       77  WS-PERIOD    PIC 9(02) VALUE ZERO.
       77  WS-SPC    PIC X(40) VALUE SPACES.
       77  WS-H1    PIC X(05) VALUE "DATE:".
       77  WS-H3           PIC X(06) VALUE "BATCH:".
       77  WS-H4           PIC X(05) VALUE "PAGE:".
       77  WS-ER1    PIC X(07) VALUE "Account".
       77  WS-ER3          PIC X(04) VALUE "Date".
       77  WS-ER4          PIC X(05) VALUE "Value".
       77  WS-ER6          PIC X(09) VALUE "No Record".
       77  WS-ER7          PIC X(13) VALUE "Group Heading".
       77  TODAY-DDMMYY    PIC 9(08) COMP-5.
       77  WS-USUB    PIC 9(04) COMP-5.

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
           03  CRT-PROGRAM         PIC  X(15) VALUE "GLP042".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "GENERAL LEDGER -DELETE RECURING ENTRY".
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
           03  FILLER      PIC X(02) VALUE "-".
           03  W10-HEAD    PIC X(25).

       COPY "W15.GL".

       01  W20-TOTALS.
           03  W20-DEBIT       PIC S9(09)V99 COMP-3.
           03  W20-CREDIT      PIC S9(09)V99 COMP-3.
           03  W20-SDT         PIC S9(07)V99 COMP-3.
           03  W20-SCR         PIC S9(07)V99 COMP-3.
           03  W20-ADJUST      PIC S9(07)V99 COMP-3.
           03  W20-RESULT      PIC S9(06)V99.
           03  W20-RES1        REDEFINES W20-RESULT.
               05  W20-WHOLE   PIC 9(06).
               05  W20-DEC     PIC S9(02).
           03  W20-TOTAL       PIC S9(07)V99 COMP-3.
    03  W20-YMD    PIC 9(08).
    03  W20-RYMD REDEFINES W20-YMD.
        05  W20-CN  PIC 9(02).
        05  W20-YR  PIC 9(02).
        05  W20-MT  PIC 9(02).
        05  W20-DY  PIC 9(02).

       01  W25-CALCS.
           03  W25-RESULT      PIC 9(05)V99.
           03  W25-RESULT1 REDEFINES W25-RESULT.
               05  W25-WHOLE   PIC 9(05).
               05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY    PIC 9(04).
               05  W25-SUB    PIC 9(01).
               05  FILLER     PIC 9(02).

       01  W25-EDIT.
           03  W25-S7V2.
               05  W25-V1      PIC Z(06)9.99CR.
          03  W25-S8V2.
              05  W25-VG1     PIC Z(07)9.99CR.
          03  W25-DATE.
              05  W25-DTE     PIC Z9/99/9999.

       01  W30-DEBT-TRN.
           03  W30-RESULT      PIC  9(04)    COMP-5.
           03  W30-TRNKEY      PIC  9(04)    COMP-5.
           03  W30-TS          PIC  9(02)    COMP-5.

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       01  W50-CALC.
           05  W50-ACNTS       OCCURS 30.
               07  W50-ANO     PIC 9(06)     COMP-3.
               07  W50-PER     PIC 9(03)V99  COMP-3.
       01  W55-TOTALS.
           03  W55-VAL         PIC S9(07)V99 COMP-3 OCCURS 12.
       01  W55-TOT             REDEFINES W55-TOTALS.
           03  W55-1           PIC S9(07)V99 COMP-3.
           03  W55-2           PIC S9(07)V99 COMP-3.
           03  W55-3           PIC S9(07)V99 COMP-3.
           03  W55-4           PIC S9(07)V99 COMP-3.
           03  W55-5           PIC S9(07)V99 COMP-3.
           03  W55-6           PIC S9(07)V99 COMP-3.
           03  W55-7           PIC S9(07)V99 COMP-3.
           03  W55-8           PIC S9(07)V99 COMP-3.
           03  W55-9           PIC S9(07)V99 COMP-3.
           03  W55-10          PIC S9(07)V99 COMP-3.
           03  W55-11          PIC S9(07)V99 COMP-3.
           03  W55-12          PIC S9(07)V99 COMP-3.
       01  W70-ACTS.
           03  W70-BRANCH      PIC 9(05)     COMP-0.

       COPY "W70DEBT.WS" REPLACING W70-DEBT BY W70-DEBREC.

       COPY "W70CRED.WS".

       01  W70-RECORD6.
           03  W70-PJF         PIC  9(05).
           03  W70-PJL         PIC  9(05).
           03  W70-RAILD       PIC S9(09)V99 COMP-3.
           03  W70-RAILM       PIC S9(09)V99 COMP-3.
           03  W70-DISCD       PIC S9(09)V99 COMP-3.
           03  W70-DISCM       PIC S9(09)V99 COMP-3.
           03  W70-COST-CODE   PIC  X(10).
           03  W70-MTHEND      PIC  9(02).
           03  W70-LPG.
               05  W70-LPP     PIC  9(02).
           03  W70-WHOLESALE   PIC  X(01).
          03  FILLER        PIC  X(30).
          03  W70-SUSGL       PIC  9(06)    COMP-3.
    03  W70-INTEGRATE   PIC  X(01).
           03  W70-DEBGL       PIC  9(06)    COMP-3.
           03  W70-CREGL       PIC  9(06)    COMP-3.
           03  W70-INTGL       PIC  9(06)    COMP-3.
           03  W70-BNKGL       PIC  9(06)    COMP-3.
           03  W70-UNPROF      PIC  9(06)    COMP-3.
           03  W70-REDGL       PIC  9(06)    COMP-3.
           03  W70-ADJGL       PIC  9(06)    COMP-3.
           03  W70-RLGL        PIC  9(06)    COMP-3.
           03  W70-DSGL        PIC  9(06)    COMP-3.
          03  W70-PL          PIC S9(09)V99 COMP-3.

       01  W75-KEYS.
           03  W75-S          PIC 9(02) COMP-5.
           03  W75-S1         PIC 9(02) COMP-5.
          03  W75-GNO       PIC 9(06) COMP-5
         OCCURS 18.

       01  W90-COMP.
           03  W90-CNAME       PIC X(40).

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

       COPY "FILE-GENL.IDS".

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
      *    ****   HEADING LINES FOR INSTRUCTION SCREEN
      *
       01  S04.
          03  LINE  2 COLUMN 26 FOREGROUND-COLOR 7 HIGHLIGHT
                                 VALUE "DELETE STANDING INSTRUCTION".
      *
      *    ****   INSTRUCTION SCREEN LAYOUT
      *
       01  S05.
          03  LINE  4 COLUMN  4 VALUE "Instruction".
    03        COLUMN 16 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(05) FROM WS-SPC.
          03  LINE  6 COLUMN  4 VALUE "Value      ".
    03        COLUMN 16 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(10) FROM WS-SPC.
          03  LINE  7 COLUMN  4 VALUE "Narrative  ".
    03        COLUMN 16 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(36) FROM WS-SPC.
          03  LINE  9 COLUMN  4 FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "Debit Details:".
          03  LINE 11 COLUMN  4 VALUE "Account No ".
    03        COLUMN 16 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(06) FROM WS-SPC.
    03        COLUMN 23 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(30) FROM WS-SPC.
          03  LINE 13 COLUMN  4 FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "Credit Details:".
          03  LINE 15 COLUMN  4 VALUE "Account No ".
    03        COLUMN 16 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(06) FROM WS-SPC.
    03        COLUMN 23 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(30) FROM WS-SPC.
      *
      *    ****   FIRST INPUT SCREEN FOR INSTRUCTION
      *
       01  S06.
    02  S06A.
          03  LINE  6 COLUMN 16 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC Z(06)9.99 USING W15-VAL AUTO.
          03  LINE  7 COLUMN 16 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC X(36) USING W15-NAR AUTO.
    02  S06B.
          03  LINE 11 COLUMN 16 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC 9(06) USING W15-ACCOUNT AUTO.
      *
      *    ****   SECOND INPUT SCREEN FOR INSTRUCTION
      *
       01  S07.
          03  LINE 15 COLUMN 16 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC 9(06) USING W15-BANK AUTO.
      *
      *    ****   INSTRUCTION NUMBER INPUT/DISPLAY SCREEN
      *
       01  S08.
          03  LINE  4 COLUMN 16 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC Z(04)9 USING W15-INSNO AUTO.

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
        MOVE "Not Authorised"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
        GO TO AA25.
            PERFORM ZA000.
      PERFORM CA000.
      CLOSE GLINST.

       AA25.
             EXIT PROGRAM.

       COPY "FUNCTION.CRT".

       COPY "LOCKED.REC".

       COPY "CLEAR.CRT".

      *
      *    ****    R E A D   F I L E S   R O U T I N E S
      *
       AB000           SECTION.

       COPY "CONTROL.RD".

       COPY "PARAM.RD".

       COPY "GACCNT.RD1".

       COPY "GLINST.RD".

       COPY "SHARED.RD".

      *                                                                 
      *     R E W R I T E   &   W R I T E   F I L E S   R O U T I N E S 
      *                                                                 
       AC000           SECTION.

       COPY "CONTROL.WR".

       COPY "PARAM.WR".

       COPY "GLINST.WR".

      /
      *    ****    LOGGING OF UPDATES
      *
       AY000           SECTION.
      *
      *    ****    Start processing transaction
      *
       AY60.
            MOVE 1   TO WS-COUNT.
            MOVE 5   TO WS-SHARED.
            PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT.
      *      MOVE SHR-STOCK  TO WS-STOCK.
      *
      *    ****    Read PARAM record 4 - Lock record and start recovery
      *            file procedure.
      *
            MOVE 4   TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
           IF PAR-USERS < 24
               MOVE 1            TO WS-SUB
        MOVE ZERO  TO WS-WAIT
               GO TO AY62.
      *
      *    ****   Q   F U L L  -  W A I T   F O R   4 S E C O N D S
      *
            DISPLAY "WAITING" AT 5051
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 12.
            COMMIT.
      ACCEPT WS-STIME FROM TIME.
      MOVE 400   TO WS-WAIT.
            PERFORM LOCK-REC-LOOP.
            DISPLAY SPACE AT 5051
       WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 3.
             GO TO AY60.

       AY61.
            MOVE "99"   TO PAR-PROG(WS-USUB).
            MOVE LS-USER  TO PAR-USR(WS-USUB).
             PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      *
      *    ****    Read the LEDGER control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *
             MOVE 4              TO WS-NETKEY.
            PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
            GO TO AY999.
      *
      *    ****   A R E   A N Y   U P D A T E S   I N P R O G R E S S
      *
       AY62.
    IF NOT (PAR-PROG(WS-SUB) = SPACES OR
     PAR-USR(WS-SUB) = SPACES)
      *
      *    ****   A R E   D E B T O R O R   S T O C K   F I L E S
      *    B E I N G   U P D A T E D
      *
        IF NOT (PAR-PROG(WS-SUB) = SPACES)
           IF PAR-PROG(WS-SUB) = "CG" OR "GL"
      *
      *    ****   Y E S   -   S E T   W A I T P E R I O D
      *
         GO TO AY62-WAIT
     ELSE
         ADD 1  TO WS-SUB
         GO TO AY62
     END-IF
        ELSE
      *
      *    ****   I S T H I S   P R O G R A M   I N T H E  Q
      *
        IF PAR-USR(WS-SUB) = LS-USER
      *
      *    ****   I S I T   N E X T I N   L I N E T O   P R O C E S S
      *
     IF WS-WAIT = ZERO
         GO TO AY63
     ELSE
         GO TO AY62-WAIT
     END-IF
        ELSE
     GO TO AY62-WAIT
        END-IF
    END-IF.
      MOVE LS-USER  TO PAR-USR(WS-SUB).
      MOVE WS-SUB  TO PAR-USERS.
      PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
      GO TO AY62-CHECK.
      *
      *    ****   S E T   W A I T   P E R I O D
      *
       AY62-WAIT.
      MOVE 300   TO WS-WAIT.
    IF NOT (PAR-USR(WS-SUB) = LS-USER)
        IF WS-SUB < 24
     ADD 1  TO WS-SUB
     GO TO AY62.

       AY62-CHECK.
    IF WS-WAIT > ZERO
        COMMIT
        DISPLAY "Waiting" AT 5072
   WITH BACKGROUND-COLOR 3
        FOREGROUND-COLOR 14 BLINK
        ACCEPT WS-STIME FROM TIME
        PERFORM LOCK-REC-LOOP
        DISPLAY "Waiting" AT 5072
   WITH BACKGROUND-COLOR 3
        FOREGROUND-COLOR 14 BLINK
        GO TO AY60.
            DISPLAY SPACE AT 5072
       WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 3.

       AY63.
            MOVE WS-SUB  TO WS-USUB.
            GO TO AY61.

       AY70.
            MOVE 4   TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK-END THRU READ-PARAM-EXIT.
      *
      *    ****    Write links back and unlock PARAM record 4 and
      *     NETWORK record 2.
      *
            PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
      MOVE 1   TO WS-USUB.

       AY72.
    IF NOT (PAR-USR(WS-USUB) = LS-USER)
        ADD 1   TO WS-USUB
        GO TO AY72.

       AY75.
            MOVE SPACES  TO PAR-PROG(WS-USUB)
              PAR-USR(WS-USUB).
    IF WS-USUB < 24
        ADD 1 WS-USUB  GIVING WS-SUB
    ELSE
        GO TO AY80.
    IF NOT (PAR-PROG(WS-SUB) = SPACES)
        MOVE PAR-PROG(WS-SUB)
     TO PAR-PROG(WS-USUB)
        MOVE PAR-USR(WS-SUB)
     TO PAR-USR(WS-USUB)
        ADD 1   TO WS-USUB
        GO TO AY75.

       AY80.
            SUBTRACT 1 FROM WS-USUB
     GIVING PAR-USERS.
            PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
            COMMIT.

       AY999.
             EXIT.
      /
      *    ****    D E L E T E   S T A N D I N G   I N S T R U C T I O N
      *
       CA000        SECTION 5.
       CA00.
             PERFORM ERASE-SCREEN.
             DISPLAY S04.
             DISPLAY S05.
            MOVE ZERO   TO W15-INSNO.
       CA05.
             ACCEPT S08.
            PERFORM CLEAR-L50.
           IF W15-INSNO = ZERO
               GO TO CA999.
            MOVE W15-INSNO  TO WS-GLINSTKEY.
             PERFORM READ-GLINST-CHECK THRU READ-GLINST-EXIT.
           IF WS-INDG = 1
              MOVE ZERO  TO WS-INDG
              MOVE WS-ER6  TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               GO TO CA05.
           IF INS-NAR = SPACES
              MOVE WS-ER6  TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               GO TO CA05.
            MOVE INS-NAR  TO W15-NAR.
            MOVE INS-VALUE  TO W15-VAL.
            MOVE INS-ACCR  TO W15-BANK G-AC.
      PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
    IF WS-F-ERROR = 8
        MOVE "INVALID ACCOUNT NUMBER"
     TO W15-ACNME
    ELSE
        MOVE G-NAME  TO W15-ACNME.
            MOVE INS-ACDT  TO W15-ACCOUNT G-AC.
      PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
    IF WS-F-ERROR = 8
        MOVE "INVALID ACCOUNT NUMBER"
     TO G-NAME.
            DISPLAY S06.
            DISPLAY S07.
      DISPLAY G-NAME AT 1123
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
      DISPLAY W15-ACNME AT 1523
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO CA00.
             PERFORM AY60 THRU AY999.
            MOVE SPACES  TO INS-NAR.
            MOVE ZERO   TO INS-VALUE INS-ACCR.
            MOVE G-GLINST  TO INS-ACDT.
            MOVE WS-GLINSTKEY  TO G-GLINST.
             PERFORM REWRITE-GLINST THRU WRITE-GLINST-EXIT.
             PERFORM AY70 THRU AY999.
             GO TO CA00.
       CA999.
             EXIT.
      /
       ZA000      SECTION 9.
       ZA00.
             PERFORM ZA55 THRU ZA60.
      MOVE LS-PARID  TO WS-PARID.
      MOVE LS-L-OR-N  TO W02-L-OR-N.
      MOVE WS-SYS-ID  TO W02-SYSID.
      MOVE LS-TODAY-DDMMYY
     TO TODAY-DDMMYY.
      MOVE LS-USUB  TO WS-USUB.
            PERFORM ERASE-SCREEN.
      *
      *    ****    D O  N O T  D I S P L A Y  E R R O R
      *     M E S S A G E S
      *
      *      MOVE 44   TO USER-NUMBER.
      *      MOVE 1   TO USER-SETTING.
      *      CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

      COPY "FUNCTION.PRO".

       ZA02.
            OPEN I-O GLINST.
           IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49.
           IF WS-STATUS NOT = "00"
               MOVE 10           TO WS-F-ERROR
               PERFORM OPEN-ERROR.
            MOVE 1 TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-COMPANY TO W90-CNAME.
             MOVE PAR-DMY TO W05-TODAY.
             MOVE PAR-YMD TO W05-TYMD.
             MOVE 6 TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-RECORD6 TO W70-RECORD6.
             MOVE ZERO TO WS-ERROR.
           IF W70-BNKGL = ZERO
               DISPLAY (20, 20) "Parameters not set up - ENTER"
                                 WITH FOREGROUND-COLOR 14
               ACCEPT (20, 50) WS-OPTION WITH FOREGROUND-COLOR 15 AUTO
               MOVE 1 TO WS-ERROR.
      DISPLAY SPACES AT 5025
       WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
            GO TO ZA999.
       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR 14.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
             ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
             GO TO ZA205.
       ZA50.
           IF WS-F-ERROR = 40
               MOVE "INTEGRATION"
                                 TO WS-FILE.
             DISPLAY "Rebuild " AT 0812
                      WS-FILE WITH FOREGROUND-COLOR 14.
             DISPLAY "Press any key to continue" AT 1012.
             ACCEPT WS-OPTION AT 1038 WITH FOREGROUND-COLOR 15.
       ZA51.
             EXIT PROGRAM.
       ZA55.
             MOVE 1 TO WS-S1.
            MOVE SPACES TO WS-MID-LNE.
       ZA60.
             MOVE WS-G1 TO WS-TCHR (WS-S1) WS-BCHR (WS-S1).
             MOVE WS-G8 TO WS-TCH  (WS-S1) WS-BCH  (WS-S1).
           IF WS-S1 < 80
               ADD 1 TO WS-S1
               GO TO ZA60.
             MOVE WS-G9  TO WS-TCH  (1).
             MOVE WS-G10 TO WS-TCH  (80).
             MOVE WS-G11 TO WS-BCH  (1).
             MOVE WS-G12 TO WS-BCH  (80).
             MOVE WS-G14 TO WS-TCHR (1)  WS-BCHR (1).
             MOVE WS-G13 TO WS-TCHR (80) WS-BCHR (80).
             MOVE WS-G2  TO WS-TCHR (11) WS-TCHR (45)
                            WS-TCHR (63).
             MOVE WS-G3  TO WS-MCHR (11) WS-MCHR (45)
                            WS-MCHR (63)
             WS-MCHR (1)  WS-MCHR (80).
             MOVE WS-G4  TO WS-BCHR (11) WS-BCHR (45)
                            WS-BCHR (63).
      MOVE LS-COMPANY  TO WS-TOP-COMP.
    IF LS-USER = LS-SYS-ID
        MOVE "SupervisorÄ"  TO WS-WRKHD
    ELSE
        MOVE "Workstation"  TO WS-WRKHD
        MOVE LS-USER    TO WS-WRKST.
       ZA200.
             DISPLAY "This program can only be run when no other worksta
      -      "tions are logged into the" AT 0803
                      WITH BACKGROUND-COLOR 4 FOREGROUND-COLOR 15.
             DISPLAY "accounting system. All other workstations should r
      -      "eturn to the program con-" AT 0903
                      WITH BACKGROUND-COLOR 4 FOREGROUND-COLOR 15.
             DISPLAY "trol menu or exit from the accounting system befor
      -      "e you may continue       " AT 1003
                      WITH BACKGROUND-COLOR 4 FOREGROUND-COLOR 15.
             DISPLAY "Files locked - Try later" AT 2312
                      WITH FOREGROUND-COLOR 14
                     " " WS-STATUS WITH FOREGROUND-COLOR 15.
            DISPLAY "Press any key" AT 5012
              WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
            ACCEPT WS-OPTION AT 5026
      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
       ZA205.
             EXIT PROGRAM.
       ZA999.
             EXIT.
      /
       ZB000      SECTION 9.

       OPEN-ERROR.
             PERFORM ERASE-SCREEN.
             DISPLAY "Open error" AT 0812 WITH FOREGROUND-COLOR 14.
             PERFORM DISPLAY-FILE-NAME.

       READ-ERROR.
             PERFORM ERASE-SCREEN.
             DISPLAY "Read error" AT 0812 WITH FOREGROUND-COLOR 14.
             PERFORM DISPLAY-FILE-NAME.

       WRITE-ERROR.
             PERFORM ERASE-SCREEN.
             DISPLAY "Write error" AT 0812 WITH FOREGROUND-COLOR 14.
             PERFORM DISPLAY-FILE-NAME.

       DISPLAY-FILE-NAME.
           IF WS-F-ERROR = 2
              MOVE W02-NETWORK  TO WS-FILE
              MOVE WS-NETKEY  TO WS-KEY
           ELSE
           IF WS-F-ERROR = 8
              MOVE W02-GACCNT  TO WS-FILE
              MOVE ZERO  TO WS-KEY
        MOVE G-AC  TO WS-KEYX
          ELSE
           IF WS-F-ERROR = 10
              MOVE W02-GLINST  TO WS-FILE
               MOVE WS-GLINSTKEY TO WS-KEY
          ELSE
           IF WS-F-ERROR = 15
              MOVE WS-PARID  TO WS-FILE
              MOVE WS-PARKEY  TO WS-KEY
          ELSE
          IF WS-F-ERROR = 37
              MOVE W02-SHARED  TO WS-FILE
              MOVE WS-SHARED  TO WS-KEY.
           IF WS-STATUS = "10"
               MOVE "End of FILE" TO WS-STAT-MESSAGE
           ELSE
           IF WS-STATUS = "22"
               MOVE "Duplicate record number" TO WS-STAT-MESSAGE
           ELSE
           IF WS-STATUS = "23"
               MOVE "Invalid record number" TO WS-STAT-MESSAGE
           ELSE
           IF WS-STATUS = "24"
               MOVE "DISK full" TO WS-STAT-MESSAGE
           ELSE
           IF WS-STATUS = "30"
               MOVE "DISK error" TO WS-STAT-MESSAGE
           ELSE
           IF WS-STATUS = "94"
               MOVE "FILE locked" TO WS-STAT-MESSAGE.
             DISPLAY "File - " AT 1012 WS-FILE WITH FOREGROUND-COLOR 11.
             DISPLAY "Status " AT 1212
                      WS-STATUS WITH FOREGROUND-COLOR 11
                     ": " WS-STAT-MESSAGE WITH FOREGROUND-COLOR 14.
           IF WS-STATUS NOT = "94"
               DISPLAY "Key    " AT 1412
                        WS-KEY WITH FOREGROUND-COLOR 11
                        " " WS-KEYX WITH FOREGROUND-COLOR 11
               DISPLAY "Reverse backup or contact program Support"
                        AT 1612.
               DISPLAY "Please make a note of these details" AT 1812.
             STOP RUN.
