      $set LINKCOUNT"384" GNT"GLP044.GNT"
      ******************************************************************
      *                                                                *
      *    ******   **       *******   ****       ** **     *
      *   ** **  **       **    **  **  **      ***       ***     *
      *   **     **       **    ** **    **    * **      * **     *
      *   **     **       ******* **    **   *  **     * **     *
      *   **   ***  **       ** **    **  *******   *******    *
      *   ** **  **       **  **  **       ** **     *
      *    ******   ********  **   ****      ****      ****    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *  G / L E D G E R   P O S T   R E C U R I N G   E N T R I E S   *
      *                                                                *
      *       Version 9.04.02 - June 2017                              *
      *               *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.        GLP044.
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

       COPY "GLTRAN.SL".

       COPY "PARAM.SL".

          SELECT PRNREP  ASSIGN W02-PRINTER
                          ORGANIZATION LINE SEQUENTIAL.
       COPY "SHARED.SL".

       DATA DIVISION.
       FILE SECTION.

       COPY "CONTROL.FDE".

       COPY "GACCNT.FDE".

       COPY "GLTRAN.FDE".

       COPY "GLINST.FD".

       COPY "PARAM.FDE".

       FD  PRNREP    LABEL RECORD OMITTED
             LINAGE IS WS-PGE-LENGTH.
       01  R-L1.
           03  R-DET       PIC X(132).
       01  R-L2.
           03  R-H1        PIC X(06).
          03  R-DTE1    PIC Z9/99/9999.
          03  FILLER    PIC X(08).
           03  R-H2        PIC X(40).
           03  R-H2A       PIC X(27).
           03  FILLER      PIC X(10).
           03  R-H3        PIC X(07).
           03  R-BTCH      PIC Z(04)9.
           03  FILLER      PIC X(10).
           03  R-H4        PIC X(06).
           03  R-PGE       PIC ZZ9.
       01  R-L3.
           03  FILLER      PIC X(03).
           03  R-H5        PIC X(11).
           03  R-H6        PIC X(05).
           03  FILLER      PIC X(01).
           03  R-H7        PIC X(13).
           03  R-H8        PIC X(05).
           03  FILLER      PIC X(06).
           03  R-H9        PIC X(09).
           03  R-H10       PIC X(12).
           03  R-H11       PIC X(13).
           03  FILLER      PIC X(01).
           03  R-H12       PIC X(09).
           03  R-H13       PIC X(13).
           03  FILLER      PIC X(08).
           03  R-H14       PIC X(14).
           03  R-H15       PIC X(09).
       01  R-L4.
           03  FILLER      PIC X(03).
           03  R-ACC       PIC 9(04)/99.
           03  FILLER      PIC X(04).
           03  R-ANME      PIC X(24).
          03  FILLER    PIC X(03).
          03  R-DTE2    PIC Z9/99/9999.
          03  FILLER    PIC X(03).
           03  R-REF       PIC Z(07).
           03  FILLER      PIC X(04).
           03  R-NAR       PIC X(36).
           03  FILLER      PIC X(01).
           03  R-DBRKO     PIC X(01).
           03  R-DBT       PIC Z(08)9.99.
           03  R-DBRKC     PIC X(01).
           03  R-CBRKO     PIC X(01).
           03  R-CRD       PIC Z(08)9.99-.
           03  R-CBRKC     PIC X(01).
           03  FILLER      PIC X(01).
       01  R-L5.
           03  RI-H1       PIC X(06).
           03  RI-H2       PIC X(40).
           03  RI-H3       PIC X(39).
           03  RI-H4       PIC X(42).
           03  RI-H5       PIC X(05).
       01  R-L6.
           03  RI-NO       PIC Z(04)9.
           03  FILLER      PIC X(01).
           03  RI-ACDT     PIC 9999/99.
           03  FILLER      PIC X(01).
           03  RI-DTNAME   PIC X(30).
           03  FILLER      PIC X(02).
           03  RI-ACCR     PIC 9999/99.
           03  FILLER      PIC X(01).
           03  RI-CRNAME   PIC X(30).
           03  FILLER      PIC X(01).
           03  RI-NAR      PIC X(36).
           03  RI-VAL      PIC Z(07)9.99.

       COPY "SHARED.FDE".

      /
       WORKING-STORAGE SECTION.
       77  WS-CHECK                  PIC  X(18)    VALUE "aeWlimemnomLalismJ".
       77  WS-SUB    PIC 9(04)  COMP-5.
       77  WS-PARKEY    PIC 9(05)   COMP-0 VALUE 1.
       77  WS-NETKEY    PIC 9(05)  COMP-0 VALUE 1.
       77  WS-GLINSTKEY    PIC 9(05)  COMP-0.
       77  WS-SHARED    PIC 9(04)  COMP-5 VALUE 1.
       77  WS-KSTORE    PIC 9(05)  COMP-0.
       77  WS-ISUB    PIC 9(05)   COMP-0.
       77  WS-CHEQUE    PIC 9(05)   COMP-0.
       77  WS-PAGE    PIC 9(05)   COMP-0.
       77  WS-MONTH    PIC 9(02) VALUE ZERO.
       77  WS-LC    PIC 9(02) VALUE ZERO.
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
       77  WS-PGE-LENGTH   PIC 9(02)  VALUE 66.
       77  WS-PRN-LENGTH   PIC 9(02)  VALUE 62.
       77  WS-COND    PIC X(06).
       77  WS-NORM    PIC X(06).
       77  WS-EXPP    PIC X(06).
       77  WS-ECAN    PIC X(06).
       77  WS-8LPI    PIC X(06).
       77  WS-6LPI    PIC X(06).
       77  WS-10CPI    PIC X(06).
       77  WS-12CPI    PIC X(06).
       77  WS-17CPI    PIC X(06).
       77  WS-BATCH    PIC 9(05)  COMP-3.
       77  WS-TNEXT        PIC 9(05)     COMP-0.
       77  WS-PERIOD       PIC  9(02)    COMP-5 VALUE ZERO.
       77  WS-PRN-NO    PIC X(01)  COMP-X VALUE 9.
       77  WS-PRN-STAT    PIC X(01)  COMP-X.
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "GLP044".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "GENERAL LEDGER -POST RECURING ENTRIES".
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

       01  W00-REC.
           03  W00-CODE    PIC  9(02).

       01  WS-PARID.
          03  WS-SYS-ID       PIC  X(03).

       01  W02-FILE-IDS.
           03  W02-PRINTER.
        05  W02-REPORT PIC X(07) VALUE "GLP044.".
               05  W02-USER   PIC X(05) VALUE SPACES.

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
           03  W75-SKEY       PIC 9(04) COMP-5
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
      PERFORM EA000.
      CLOSE GLINST
     PRNREP.

       AA25.
             EXIT PROGRAM.

       COPY "AA900.ALM".

       COPY "FUNCTION.SCR".

       COPY "ERROR.SCR".

       COPY "OPTION.CRT".

       COPY "LOCKED.REC".

       COPY "CLEAR.CRT".

      *
      *    ****    R E A D   F I L E S   R O U T I N E S
      *
       AB000           SECTION.

       COPY "CONTROL.RD".

       COPY "PARAM.RD".

       COPY "GACCNT.RD1".

       COPY "GLTRAN.RD".

       COPY "GLINST.RD".

       COPY "SHARED.RD".

      *                                                                 
      *     R E W R I T E   &   W R I T E   F I L E S   R O U T I N E S 
      *                                                                 
       AC000           SECTION.

       COPY "CONTROL.WR".

       COPY "PARAM.WR".

       COPY "GACCNT.WR".

       COPY "GLINST.WR".

       COPY "GLTRAN.WR".

      *
      *   ****    CALCULATE THE PERIOD INTO WHICH THIS DATE FALLS
      *
       AE000           SECTION.
       AE00.
            MOVE ZERO   TO WS-ERROR.
           IF W15-DD < 01 OR > 31
               GO TO AE10.
           IF W15-MM < 01 OR > 12
               GO TO AE10.
           IF W15-MM = 01 OR 03 OR 05 OR 07 OR 08 OR 10 OR 12
               GO TO AE15.
           IF W15-DD > 30
               GO TO AE10.
           IF W15-MM = 02 AND W15-DD > 28
               GO TO AE05.
             GO TO AE15.

       AE05.
           IF W15-DD > 29
               GO TO AE10.
            DIVIDE W15-CY BY 4  GIVING W20-RESULT.
           IF W20-DEC = ZERO
               GO TO AE15.

       AE10.
            MOVE 1   TO WS-ERROR.
             GO TO AE999.
       AE15.
      MOVE W15-MM  TO WS-PERIOD.
    IF W15-CY > W05-CY
        GO TO AE10.
    IF W15-CY < W05-CY
               GO TO AE20.
    IF W05-MM = W15-MM
       GO TO AE999.
    IF W15-MM > W05-MM
        GO TO AE10.
    IF W15-MM > WS-Y-END
               GO TO AE999.
    IF W15-CY > WS-L-END
               GO TO AE999.
      ADD 12   TO WS-PERIOD
             GO TO AE999.

       AE20.
    IF W15-CY > WS-L-END
               GO TO AE999.
      ADD 12   TO WS-PERIOD.

       AE999.
             EXIT.
      /
      *    THIS ROUTINE UPDATES THE VARIOUS MONTHS FROM TRANSACTION
      *    DATE UP TO THE CURRENT MONTH
      *
       AH000           SECTION.
       AH00.
           IF WS-PERIOD = W05-MM
               GO TO AH999.
           IF WS-PERIOD > 12
               SUBTRACT 13 FROM WS-PERIOD GIVING WS-S3
               PERFORM AH10
               ADD 1 WS-OPEN GIVING WS-MS1
           ELSE
               MOVE WS-PERIOD TO WS-MS1.
       AH05.
             ADD WS-VALUE        TO G-VAL (WS-MS1).
             ADD 1               TO WS-MS1.
           IF WS-MS1 > 12
               MOVE 1            TO WS-MS1.
           IF WS-MS1 = W05-MM
               GO TO AH999.
             GO TO AH05.
       AH10.
             ADD 1               TO WS-S3.
           IF WS-S3 > 12
               MOVE 1            TO WS-S3.
             ADD WS-VALUE        TO G-LVAL (WS-S3).
           IF WS-S3 NOT = WS-OPEN
               GO TO AH10.
      ADD WS-VALUE  TO G-OBAL.
       AH15.
             EXIT.
       AH999.
             EXIT.

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

       AY65.
             MOVE 4              TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
             MOVE G-BATCH        TO WS-BATCH.
           IF G-BATCH = 99999
               MOVE 1            TO G-BATCH
           ELSE
               ADD 1             TO G-BATCH.
             PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
             GO TO AY999.

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
      *    ****    P O S T   S T A N D I N G   E N T R I E S
      *
       EA000        SECTION 5.
       EA00.
             PERFORM ERASE-SCREEN.
             DISPLAY "Post Standing Instructions" AT 0228
                      WITH FOREGROUND-COLOR 15.
             DISPLAY "Are you sure (Y/N)?" AT 0412
                      WITH FOREGROUND-COLOR 14.
       EA02.
             ACCEPT WS-OPTION AT 0432 WITH FOREGROUND-COLOR 15 AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
    IF NOT (WS-OPTION = "N" OR "Y")
        GO TO EA02.
          IF WS-OPTION = "N"
               GO TO EA999.
             MOVE 4              TO WS-NETKEY.
             MOVE 1              TO WS-GLINSTKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
            MOVE W05-MM  TO W15-MONTH.
            DISPLAY "Post for Month-" AT 0612
        WITH FOREGROUND-COLOR 3 HIGHLIGHT
       "Keyin 00 to EXIT" AT 0630
        WITH FOREGROUND-COLOR 6 HIGHLIGHT.
       EA05.
            ACCEPT W15-MONTH AT 0627
            WITH FOREGROUND-COLOR 15 UPDATE AUTO.
    IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
        GO TO EA999
   WHEN OTHER
                     PERFORM AA900-ALARM
       GO TO EA05
        END-EVALUATE.
          IF W15-MONTH = ZERO
               GO TO EA999.
           IF W15-MONTH > 12
               GO TO EA05.
           IF G-INST = 12
              MOVE 1   TO W00-CODE
           ELSE
              ADD 1 G-INST  GIVING W00-CODE.
            PERFORM CLEAR-L50.
           IF G-INST = W15-MONTH
              MOVE "Already Posted"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               GO TO EA05.
           IF W00-CODE = W15-MONTH
               GO TO EA08.
      MOVE SPACES  TO WS-ERR-MES.
            STRING "Month should be - "
       DELIMITED SIZE
             W00-CODE DELIMITED SIZE
            " use Month keyed in (Y/N)?"
       DELIMITED SIZE
     INTO WS-ERR-MES.
      MOVE SPACE   TO WS-OPTION.
            PERFORM OPT-MESSAGE.
          IF WS-OPTION = "N"
               GO TO EA05.

       EA08.
             PERFORM AY60 THRU AY999.
             PERFORM AY65 THRU AY999.
            MOVE W15-MONTH  TO G-INST W15-SMTH.

       EA10.
            MOVE ZERO   TO WS-PAGE W20-DEBIT W20-CREDIT.
            MOVE "G/L STANDING JOURNALS"
     TO W10-HEAD.
            MOVE ZERO   TO W15-REFER.
            MOVE SPACES  TO W15-NAR.
            MOVE W05-TODAY  TO W15-DATE.
            MOVE WS-COND  TO R-DET.
             WRITE R-L1 BEFORE 0 LINES.

       EA15.
            MOVE SPACES  TO R-DET.
             MOVE WS-H1          TO R-H1.
             MOVE W90-COMP       TO R-H2.
             MOVE W10-H2         TO R-H2A.
             MOVE WS-H3          TO R-H3.
             MOVE WS-H4          TO R-H4.
             ADD 1               TO WS-PAGE.
             MOVE WS-PAGE        TO R-PGE.
             MOVE WS-BATCH       TO R-BTCH.
             MOVE W05-TODAY      TO R-DTE1.
             WRITE R-L1 BEFORE 2 LINES.
             MOVE SPACES         TO R-DET.
            MOVE "ACCNT NO"  TO R-H5.
            MOVE ALL "_"  TO R-H6.
             MOVE "ACCOUNT NAME" TO R-H7.
            MOVE ALL "_"  TO R-H8.
            MOVE "DATE"  TO R-H9.
            MOVE "REFERENCE"  TO R-H10.
            MOVE ALL "_"  TO R-H11.
            MOVE "NARRATION"  TO R-H12.
            MOVE ALL "_"  TO R-H13.
            MOVE "DEBIT"  TO R-H14.
            MOVE "CREDIT"  TO R-H15.
             WRITE R-L1 BEFORE 2 LINES.
            MOVE SPACES  TO R-DET.
       EA20.
             PERFORM READ-GLINST-CHECK THRU READ-GLINST-EXIT.
           IF WS-INDG = 1
              MOVE ZERO  TO WS-INDG
               GO TO EA90.
           IF INS-NAR = SPACES
               GO TO EA85.
            MOVE WS-GLINSTKEY  TO W15-SINST.
            MOVE INS-NAR  TO W15-NAR.
            MOVE INS-VALUE  TO W15-VAL.
            MOVE INS-ACCR  TO W15-BANK.
            MOVE INS-ACDT  TO W15-ACCOUNT.
      *
      *    ****    PROCESS DEBIT ENTRY
      *
            MOVE W15-ACCOUNT  TO R-ACC.
            MOVE W15-NAR  TO R-NAR.
            MOVE W15-REFER  TO R-REF.
            MOVE W15-DATE  TO R-DTE2.
            MOVE W15-VAL  TO R-DBT.

       EA25.
             MOVE 1601           TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN > 23.
      MOVE W15-ACCOUNT  TO G-AC.
      PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
    IF WS-F-ERROR = 0
              GO TO EA40.
            DISPLAY WS-ER6 AT 1612
       WITH FOREGROUND-COLOR 6 HIGHLIGHT
            " Debit Ac " WITH FOREGROUND-COLOR 6 HIGHLIGHT
             W15-ACCOUNT WITH FOREGROUND-COLOR 7 HIGHLIGHT
            " Instruction " WITH FOREGROUND-COLOR 6 HIGHLIGHT
             W15-SINST WITH FOREGROUND-COLOR 7 HIGHLIGHT.
            DISPLAY "Must entry be ignored (" AT 1812
       "Y" WITH FOREGROUND-COLOR 6 HIGHLIGHT "/"
       "N" WITH FOREGROUND-COLOR 6 HIGHLIGHT ")?".
            ACCEPT WS-OPTION AT 1841
      WITH FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 5 AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
          IF NOT (WS-OPTION = "N")
               GO TO EA35.
            DISPLAY "Debit Account :" AT 2012.

       EA30.
            ACCEPT W15-ACCOUNT AT 2028
            WITH FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 5 UPDATE AUTO.
           IF W15-ACCOUNT < 100000
              MOVE "Invalid account number"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               GO TO EA30.
           IF W15-ASER = ZERO
              MOVE WS-ER7  TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               GO TO EA30.
             GO TO EA25.

       EA35.
            MOVE "*** NO ACCOUNT - ENTRY IGNORED ***"
     TO R-ANME.
             GO TO EA55.

       EA40.
            MOVE G-NAME  TO R-ANME.

       EA45.
             INITIALIZE T-REC.

       EA50.
      MOVE W15-CC  TO W15-CN.
             MOVE W15-YY         TO W15-YR.
             MOVE W15-MM         TO W15-MT.
             MOVE W15-DD         TO W15-DY.
             MOVE W15-YMD        TO T-DATE.
             MOVE W15-NAR        TO T-NARATIVE.
             MOVE W15-REFER      TO T-REF.
             MOVE W15-VAL        TO T-VAL.
             MOVE G-AC           TO T-AC.
           IF W15-VAL < ZERO
               ADD W15-VAL       TO W20-CREDIT
               ADD W15-VAL       TO G-CR
               MOVE 02           TO T-CODE
           ELSE
               ADD W15-VAL       TO W20-DEBIT
               ADD W15-VAL       TO G-DT
               MOVE 01           TO T-CODE.
             MOVE W15-VAL        TO WS-VALUE.
             MOVE WS-BATCH       TO T-BATCH.
      MOVE "N"   TO T-FLAG.
             PERFORM AE000.
             PERFORM AH000.
             ADD WS-VALUE        TO G-VAL (W05-MM).
             ADD W15-VAL         TO G-BAL.
             PERFORM WRITE-GLTRAN THRU WRITE-GLTRAN-EXIT.
             PERFORM REWRITE-GACCNT THRU WRITE-GACCNT-EXIT.
             ADD 1               TO G-GLTRAN.

       EA55.
          IF LINAGE-COUNTER < WS-PRN-LENGTH
               WRITE R-L1 BEFORE 1 LINES
           ELSE
               WRITE R-L1 BEFORE PAGE
               PERFORM EA15.
            MOVE SPACES  TO R-DET.
      *
      *    ****    PROCESS CREDIT ENTRY
      *
       EA60.
            MULTIPLY -1  BY W15-VAL.
            MOVE W15-BANK  TO W15-ACCOUNT R-ACC.
            MOVE W15-NAR  TO R-NAR.
            MOVE W15-REFER  TO R-REF.
            MOVE W15-DATE  TO R-DTE2.
            MOVE W15-VAL  TO R-CRD.

       EA65.
             MOVE 1601           TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN > 23.
      MOVE W15-ACCOUNT  TO G-AC.
      PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
    IF WS-F-ERROR = 0
              GO TO EA80.
            DISPLAY WS-ER6 AT 1612
       WITH FOREGROUND-COLOR 6 HIGHLIGHT
            " Credit Ac " WITH FOREGROUND-COLOR 6 HIGHLIGHT
             W15-ACCOUNT WITH FOREGROUND-COLOR 7 HIGHLIGHT
            " Instruction " WITH FOREGROUND-COLOR 6 HIGHLIGHT
             W15-SINST WITH FOREGROUND-COLOR 7 HIGHLIGHT .
            DISPLAY "Must entry be ignored (" AT 1812
       "Y" WITH FOREGROUND-COLOR 6 HIGHLIGHT "/"
       "N" WITH FOREGROUND-COLOR 6 HIGHLIGHT ")?".
            ACCEPT WS-OPTION AT 1842
      WITH FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 5 AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
          IF NOT (WS-OPTION = "N")
               GO TO EA75.
            DISPLAY "Credit Account :" AT 2012.

       EA70.
            ACCEPT W15-ACCOUNT AT 2029
            WITH FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 5 UPDATE AUTO.
          IF W15-ACCOUNT < 100000
              MOVE "Invalid account number"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               GO TO EA70.
           IF W15-ASER = ZERO
              MOVE WS-ER7  TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               GO TO EA70.
             GO TO EA65.

       EA75.
             MOVE "*** NO ACCOUNT - ENTRY IGNORED ***" TO R-ANME.
             PERFORM EA55.
             GO TO EA85.

       EA80.
            MOVE G-NAME  TO R-ANME.
             PERFORM EA45 THRU EA55.

       EA85.
            ADD 1   TO WS-GLINSTKEY.
             GO TO EA20.

       EA90.
             PERFORM AY70 THRU AY999.
            ADD W20-DEBIT W20-CREDIT
     GIVING W20-TOTAL.
          IF LINAGE-COUNTER < (WS-PRN-LENGTH - 1)
               WRITE R-L1 BEFORE 1 LINES
           ELSE
               WRITE R-L1 BEFORE PAGE
               PERFORM EA15.
           IF W20-TOTAL NOT = ZERO
              MOVE "***  OUT OF BALANCE  ***"
     TO R-H2.
            MOVE "***** TOTALS *****"
     TO R-NAR.
            MOVE W20-DEBIT  TO R-DBT.
            MOVE W20-CREDIT  TO R-CRD.
             WRITE R-L1 BEFORE PAGE.
            MOVE WS-NORM  TO R-DET.
             WRITE R-L1 BEFORE 0 LINES.
           IF (W20-DEBIT = ZERO) AND (W20-CREDIT = ZERO)
               GO TO EA999.
           IF G-BATCH = 99999
               MOVE 1            TO G-BATCH
           ELSE
               ADD 1             TO G-BATCH.

       EA999.
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
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

      COPY "FUNCTION.PRO".

    IF LS-USER NUMERIC
        IF LS-USE > ZERO AND < 110
     MOVE LS-USE  TO WS-USUB
                   GO TO ZA01A.
             MOVE 110            TO WS-USUB.
       ZA01A.
             ADD 2549 WS-USUB    GIVING W25-RESULT.
             MOVE W25-KEY        TO WS-PARKEY.
             ADD 1 W25-SUB       GIVING WS-S6.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
    IF TRM-STDP (WS-S6) = 1
               MOVE "LPT1"       TO W02-PRINTER
        MOVE ZERO  TO WS-PRN-NO
           ELSE
           IF TRM-STDP (WS-S6) = 2
               MOVE "LPT2"       TO W02-PRINTER
        MOVE 1   TO WS-PRN-NO
           ELSE
           IF TRM-STDP (WS-S6) = 3
               MOVE "LPT3"       TO W02-PRINTER
        MOVE 2   TO WS-PRN-NO
    ELSE
           IF TRM-STDP (WS-S6) = 4
               MOVE "COM1"       TO W02-PRINTER
           ELSE
           IF TRM-STDP (WS-S6) = 5
               MOVE "COM2"       TO W02-PRINTER
           ELSE
           IF TRM-STDP (WS-S6) = 9
        MOVE LS-USER  TO W02-USER.
             ADD 501 TRM-PRN3 (WS-S6)
                                 GIVING W25-RESULT.
             DIVIDE W25-RESULT BY 2
                                 GIVING WS-PARKEY
                                 REMAINDER WS-S6.
             ADD 1               TO WS-S6.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE TRM-CONP (WS-S6)
                                 TO WS-COND.
             MOVE TRM-NORP (WS-S6)
                                 TO WS-NORM.
             MOVE TRM-EXPP (WS-S6)
                                 TO WS-EXPP.
             MOVE TRM-ECAN (WS-S6)
                                 TO WS-ECAN.
             MOVE TRM-8LPI (WS-S6)
                                 TO WS-8LPI.
             MOVE TRM-6LPI (WS-S6)
                                 TO WS-6LPI.
             MOVE TRM-10CPI (WS-S6)
                                 TO WS-10CPI.
             MOVE TRM-12CPI (WS-S6)
                                 TO WS-12CPI.
             MOVE TRM-17CPI (WS-S6)
                                 TO WS-17CPI.
      MOVE TRM-LENGTH (WS-S6)
     TO WS-PRN-LENGTH.
      MOVE TRM-PAGE (WS-S6)
     TO WS-PGE-LENGTH.
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
            MOVE 1   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
            MOVE PAR-COMPANY  TO W90-CNAME.
            MOVE PAR-DMY  TO W05-TODAY.
            MOVE PAR-YMD  TO W05-TYMD.
            MOVE 6   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
            MOVE PAR-RECORD6  TO W70-RECORD6.
      MOVE 4   TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
            MOVE G-BATCH  TO WS-BATCH.
           IF G-YEAR-END = ZERO
              MOVE 2   TO G-YEAR-END
              PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
      *
      *    ****    S E T   U P   M O N T H S   ( Y E A R   E N D)
      *
            MOVE 1   TO WS-S1.
            MOVE G-YEAR-END  TO WS-S2 WS-Y-END.
            MOVE G-LAST-YE  TO WS-L-END.
    IF WS-L-END = ZERO
        SUBTRACT 1 FROM W05-YY
     GIVING WS-L-END.
    IF G-OPEN = ZERO
        MOVE WS-Y-END  TO G-OPEN.
      MOVE G-OPEN  TO WS-OPEN.

       ZA15.
            MOVE ZERO   TO WS-ERROR.
           IF W70-BNKGL = ZERO
              DISPLAY "Parameters not set up - ENTER" AT 2020
         WITH FOREGROUND-COLOR 14
               ACCEPT (20, 50) WS-OPTION WITH FOREGROUND-COLOR 15 AUTO
               MOVE 1 TO WS-ERROR.
    IF WS-PRN-NO < 3
        PERFORM ZA210 THRU ZA215
        IF WS-ERROR = 1
     GO TO ZA205
    ELSE
        DISPLAY "Waiting for printer response" AT 1412
   WITH FOREGROUND-COLOR 14 BLINK.
            OPEN OUTPUT PRNREP.
      CALL X"91" USING X91-RES X91-FUN PRNREP.
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
             MOVE SPACES TO WS-MID-LNE WS-MID-LNE2.

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
                            WS-MCHR (1)  WS-MCHR (80)
                            WS-MCH  (1)  WS-MCH  (80).
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

       ZA210.
      MOVE ZERO   TO WS-ERROR
        WS-S1 WS-S2 WS-S3 WS-S4
        WS-S5 WS-S6 WS-S7 WS-S8.
      CALL "PC_TEST_PRINTER" USING WS-PRN-NO
       WS-PRN-STAT
        RETURNING WS-STATUS.
    IF WS-PRN-STAT > 127
        MOVE 1   TO WS-S1
        SUBTRACT 128  FROM WS-PRN-STAT.
    IF WS-PRN-STAT > 63
        MOVE 1   TO WS-S2
        SUBTRACT 64  FROM WS-PRN-STAT.
    IF WS-PRN-STAT > 31
        MOVE 1   TO WS-S3
        SUBTRACT 32  FROM WS-PRN-STAT.
    IF WS-PRN-STAT > 15
        MOVE 1   TO WS-S4
        SUBTRACT 16  FROM WS-PRN-STAT.
    IF WS-PRN-STAT > 7
        MOVE 1   TO WS-S5
        SUBTRACT 8  FROM WS-PRN-STAT.
    IF WS-PRN-STAT > 3
        MOVE 1   TO WS-S6
        SUBTRACT 4  FROM WS-PRN-STAT.
    IF WS-PRN-STAT > 1
        MOVE 1   TO WS-S7
        SUBTRACT 2  FROM WS-PRN-STAT.
      MOVE WS-PRN-STAT  TO WS-S8.
    IF WS-S3 = 0 AND
       WS-S5 = 0 AND
       WS-S8 = 0
        GO TO ZA215.
      DISPLAY "Check printer - " AT 2304
        WITH FOREGROUND-COLOR 6 HIGHLIGHT
       "R" WITH FOREGROUND-COLOR 7 HIGHLIGHT
       "etry - "
       "A" WITH FOREGROUND-COLOR 7 HIGHLIGHT
       "bort".
       ZA212.
      ACCEPT WS-OPTION AT 2334
      WITH FOREGROUND-COLOR 7 HIGHLIGHT AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
      IF NOT (WS-OPTION = "A" OR "R")
   GO TO ZA212.
      IF WS-OPTION = "A"
   MOVE 1   TO WS-ERROR
   GO TO ZA215.
        GO TO ZA210.

       ZA215.
      EXIT.

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
           IF WS-F-ERROR = 11
               MOVE W02-GLTRAN   TO WS-FILE
               MOVE ZERO         TO WS-KEY
               MOVE T-KEY        TO WS-KEYX
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
