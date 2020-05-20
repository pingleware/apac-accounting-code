      $set LINKCOUNT"512" GNT"GLP094.GNT"
      ******************************************************************
      *                                                                *
      *    ******   **       *******   ****    ****** **     *
      *   ** **  **       **    **  **  **   ** **     ***     *
      *   **     **       **    ** **    **  ** **    * **     *
      *   **     **       ******* **    **   *******   * **     *
      *   **   ***  **       ** **    ** **  *******    *
      *   ** **  **       **  **  **   ** ** **     *
      *    ******   ********  **   ****    ******      ****    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     G / L E D G E R   D E L E T E   A C C O U N T        *
      *                                                                *
      *       Version 9.04.02 - June 2017                              *
      *               *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.        GLP094.
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

       COPY "GLTRAN.SL".

       COPY "GLJRN.SL".

       COPY "LEDTRN.SL".

       COPY "RECOVER.SL".

       COPY "SHARED.SL".

       COPY "TXTRAN.SL".

       DATA DIVISION.
       FILE SECTION.

       COPY "CONTROL.FDE".

       COPY "PARAM.FDE".

       COPY "GACCNT.FDE".

       COPY "GLTRAN.FDE".

       COPY "GLJRN.FD".

       COPY "LEDTRN.FDE".

       COPY "RECOVER.FD".

       COPY "SHARED.FDE".

       COPY "TXTRAN.FDE".

      /
       WORKING-STORAGE SECTION.
       77  WS-CHECK    PIC X(18)  VALUE
      "aeWlimemnomLalismJ".
       77  WS-IXS     PIC  9(04)   COMP-5.
       77  WS-IXS1          PIC  9(04)    COMP-5.
       77  WS-IXS2          PIC  9(04)    COMP-5.
       77  WS-IXS3          PIC  9(04)    COMP-5.
       77  WS-IXS4          PIC  9(04)    COMP-5.
       77  WS-PARKEY    PIC 9(04)  COMP-5 VALUE 1.
       77  WS-LTRKEY    PIC 9(06)  COMP-5 VALUE 0.
       77  WS-ENQPOS    PIC 9(06)  COMP-5.
       77  WS-ENQEND       PIC  9(06)    COMP-5.
       77  WS-NETKEY       PIC  9(04)    COMP-5 VALUE 1.
       77  WS-KEYSTR       PIC  9(06)    COMP-5.
       77  WS-KSTORE       PIC  9(06)    COMP-5.
       77  WS-SKSTORE      PIC  9(06)    COMP-5.
       77  WS-INKEY        PIC  9(06)    COMP-5.
       77  WS-COKEY        PIC  9(06)    COMP-5.
       77  WS-GLJSTR       PIC  9(04)    COMP-5.
       77  WS-GLJKEY       PIC  9(06)    COMP-5.
       77  WS-RECKEY       PIC  9(04)    COMP-5.
       77  WS-RECOVER      PIC  9(04)    COMP-5.
       77  WS-SHARED    PIC 9(04)  COMP-5 VALUE 1.
       77  WS-TRANS        PIC  9(04)    COMP-5 VALUE 1.
       77  WS-ISUB         PIC  9(04)    COMP-5.
       77  WS-CHEQUE       PIC  9(04)    COMP-5.
       77  WS-PAGE         PIC  9(04)    COMP-5.
       77  WS-LINES        PIC  9(04)    COMP-5 VALUE 66.
       77  WS-SUB          PIC  9(04)    COMP-5.
       77  WS-S            PIC  9(04)    COMP-5.
       77  WS-S1           PIC  9(04)    COMP-5.
       77  WS-S2           PIC  9(04)    COMP-5.
       77  WS-S3           PIC  9(04)    COMP-5.
       77  WS-S4    PIC 9(04)  COMP-5.
       77  WS-S6           PIC  9(04)    COMP-5.
       77  WS-S9    PIC 9(04)   COMP-5.
       77  WS-Y-END        PIC  9(02) VALUE ZERO.
       77  WS-L-END    PIC 9(04) VALUE ZERO.
       77  WS-OPEN         PIC  9(02) VALUE ZERO.
       77  WS-MS1          PIC  9(04)    COMP-5.
       77  WS-MS2          PIC  9(04)    COMP-5.
       77  WS-VALUE        PIC S9(09)V99 COMP-3.
       77  WS-EXS          PIC  9(01).
       77  WS-CONREC       PIC  9(01) VALUE ZERO.
       77  WS-INDG         PIC  9(01) VALUE ZERO.
       77  WS-OPTION       PIC  X(01).
       77  WS-SKIP         PIC  X(01) VALUE "N".
       77  WS-FIRST        PIC  9(02) VALUE ZERO.
       77  WS-LAST         PIC  9(02) VALUE ZERO.
       77  WS-ERROR        PIC  9(01) VALUE ZERO.
       77  WS-NUM          PIC  Z9.
       77  WS-NORM         PIC  X(04).
       77  WS-COND         PIC  X(04).
       77  WS-EXPP         PIC  X(04).
       77  WS-ECAN         PIC  X(04).
       77  WS-8LPI    PIC X(04).
       77  WS-6LPI    PIC X(04).
       77  WS-10CPI        PIC  X(04).
       77  WS-12CPI        PIC  X(04).
       77  WS-17CPI        PIC  X(04).
       77  WS-DBLP         PIC  X(04).
       77  WS-DCAN         PIC  X(04).
       77  WS-BATCH        PIC  9(05)    COMP-3.
       77  WS-TNEXT        PIC  9(04)    COMP-5.
       77  WS-PERIOD       PIC  9(02)    COMP-5 VALUE ZERO.
       77  WS-ER1    PIC X(07) VALUE "Account".
       77  WS-ER3          PIC  X(04) VALUE "Date".
       77  WS-ER4          PIC  X(05) VALUE "Value".
       77  WS-ER5          PIC  X(13) VALUE "Cheque Number".
       77  WS-ER6          PIC  X(09) VALUE "No Record".
       77  WS-ER7          PIC  X(13) VALUE "Group Heading".
       77  PRG-NAME                PIC  X(12)           VALUE SPACES.
       77  PRG-GLED-LUP            PIC  X(12)           VALUE "GLP\GLPLOOK".
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "GLP094".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "GENERAL LEDGER -DELETE ACCOUNT".
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

       01  W02-FILE-IDS.
           03  W02-PRINTER.
        05  W02-REPORT PIC X(07) VALUE "GLP002.".
               05  W02-USER   PIC X(05) VALUE SPACES.

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
           03  W25-RESULT      PIC 9(05)V99.
           03  W25-RESULT1 REDEFINES W25-RESULT.
               05  W25-WHOLE   PIC 9(05).
               05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY    PIC 9(04).
               05  W25-SUB    PIC 9(01).
               05  FILLER     PIC 9(02).

       01  W25-EDIT.
           03  W25-7.
               05  W25-1   PIC  ZZZZZZZZ9-.
           03  W25-S7V2.
               05  W25-V1  PIC  Z(07)9.99CR.
          03  W25-S8V2.
              05  W25-VG1     PIC Z(07)9.99CR.
          03  W25-DATE.
              05  W25-DTE PIC Z9/99/9999.
           03  W25-REF.
               05  W25-REFER
                           PIC Z(07).
           03  W25-RECKEY  PIC  Z(04)9.
       01  W30-MONTH-NAMES.
           03  W30-MONTHS.
               05  FILLER  PIC  X(24) VALUE
                           "Jan:Feb:Mar:Apr:May:Jun:".
               05  FILLER  PIC X(24) VALUE
                           "Jul:Aug:Sep:Oct:Nov:Dec:".
           03  W30-MNTHS REDEFINES W30-MONTHS.
               05  W30-MONTH OCCURS 12.
                   07  W30-MTH PIC X(03).
                   07  FILLER  PIC X(01).
       01  W35-MONTH-NAMES.
           03  W35-MONTHS.
               05  W35-MONTH OCCURS 12.
                   07  W35-MTH PIC X(03).
                   07  FILLER  PIC X(01).
           03  W35-MTHS REDEFINES W35-MONTHS.
               05  W35-MTH1.
                   07  W35-M1  PIC X(03).
                   07  FILLER  PIC X(01).
               05  W35-MTH2.
                   07  W35-M2  PIC X(03).
                   07  FILLER  PIC X(01).
               05  W35-MTH3.
                   07  W35-M3  PIC X(03).
                   07  FILLER  PIC X(01).
               05  W35-MTH4.
                   07  W35-M4  PIC X(03).
                   07  FILLER  PIC X(01).
               05  W35-MTH5.
                   07  W35-M5  PIC X(03).
                   07  FILLER  PIC X(01).
               05  W35-MTH6.
                   07  W35-M6  PIC X(03).
                   07  FILLER  PIC X(01).
               05  W35-MTH7.
                   07  W35-M7  PIC X(03).
                   07  FILLER  PIC X(01).
               05  W35-MTH8.
                   07  W35-M8  PIC X(03).
                   07  FILLER  PIC X(01).
               05  W35-MTH9.
                   07  W35-M9  PIC X(03).
                   07  FILLER  PIC X(01).
               05  W35-MTH10.
                   07  W35-M10 PIC X(03).
                   07  FILLER  PIC X(01).
               05  W35-MTH11.
                   07  W35-M11 PIC X(03).
                   07  FILLER  PIC X(01).
               05  W35-MTH12.
                   07  W35-M12 PIC X(03).
                   07  FILLER  PIC X(01).

       01  W40-CUR-YEAR.
          03  W40-YEAR    PIC 9(04) COMP-5 OCCURS 12.

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       01  W50-CALC.
           05  W50-ACNTS   OCCURS 30.
               07  W50-ANO PIC  9(06)    COMP-3.
               07  W50-PER PIC S9(03)V99 COMP-3.
       01  W55-TOTALS.
           03  W55-VAL     PIC S9(09)    COMP-3 OCCURS 12.
       01  W55-TOT         REDEFINES W55-TOTALS.
           03  W55-1       PIC S9(09)    COMP-3.
           03  W55-2       PIC S9(09)    COMP-3.
           03  W55-3       PIC S9(09)    COMP-3.
           03  W55-4       PIC S9(09)    COMP-3.
           03  W55-5       PIC S9(09)    COMP-3.
           03  W55-6       PIC S9(09)    COMP-3.
           03  W55-7       PIC S9(09)    COMP-3.
           03  W55-8       PIC S9(09)    COMP-3.
           03  W55-9       PIC S9(09)    COMP-3.
           03  W55-10      PIC S9(09)    COMP-3.
           03  W55-11      PIC S9(09)    COMP-3.
           03  W55-12      PIC S9(09)    COMP-3.
       01  W55-TOTA        REDEFINES W55-TOTALS.
          03  W55-1A        PIC S9(07)V99 COMP-3.
          03  W55-2A        PIC S9(07)V99 COMP-3.
          03  W55-3A        PIC S9(07)V99 COMP-3.
          03  W55-4A        PIC S9(07)V99 COMP-3.
          03  W55-5A        PIC S9(07)V99 COMP-3.
          03  W55-6A        PIC S9(07)V99 COMP-3.
          03  W55-7A        PIC S9(07)V99 COMP-3.
          03  W55-8A        PIC S9(07)V99 COMP-3.
          03  W55-9A        PIC S9(07)V99 COMP-3.
          03  W55-10A        PIC S9(07)V99 COMP-3.
          03  W55-11A        PIC S9(07)V99 COMP-3.
          03  W55-12A        PIC S9(07)V99 COMP-3.
       01  W55-TOTB        REDEFINES W55-TOTALS.
          03  W55-VALA    PIC S9(07)V99 COMP-3 OCCURS 12.
       01  W60-TOTALS.
           03  W60-VAL     PIC S9(09)    COMP-3 OCCURS 12.
       01  W65-TOTALS.
           03  W65-VAL     PIC S9(09)    COMP-3 OCCURS 12.
       01  W70-ACTS.
           03  W70-BRANCH  PIC  9(04)    COMP-5.

       COPY "W70.WS".

       01  W75-KEYS.
           03  W75-S          PIC 9(02) COMP-5.
           03  W75-S1         PIC 9(02) COMP-5.
    03  W75-GNO       PIC 9(06) COMP-5
         OCCURS 18.

       01  W90-COMP.
           03  W90-CNAME   PIC  X(40).

       LINKAGE SECTION.

       COPY "CHAIN.LS".

       COPY "FILE-GENL.IDS".

      /
       SCREEN SECTION.

       01  S11.
          03  LINE  3 COLUMN 30 FOREGROUND-COLOR 7 HIGHLIGHT
                                 VALUE "DELETE  G/L ACCOUNT".
           03  LINE  5 COLUMN 16 VALUE "Account Number".
           03  LINE  6 COLUMN 16 VALUE "Account Name".

       01  S17.
          03  FOREGROUND-COLOR 3 HIGHLIGHT
      PIC 9(06) USING W15-ACCOUNT AUTO.

       COPY "LEDLUP.CRT".

      /
       PROCEDURE DIVISION
   USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
       AA000           SECTION.
       AA00.
             PERFORM ZA000.
      PERFORM HA000.
      CLOSE RECOVER.

       AA49.
      EXIT PROGRAM.

       COPY "FUNCTION.CRT".

       COPY "LOCKED.REC".

       COPY "CLEAR.CRT".

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ This routine is used to call  the General Ledger Accounts ³
      *    ³ lookup program and cancel it when control is returned.    ³
      *    ³ The  relevant  error  message  will be  displayed  if  an ³
      *    ³ exception occurs. The usual  parameters are passed to the ³
      *    ³ lookup program as well W15-GLEDGER which will contain the ³
      *    ³ details of the account that has been selected when control³
      *    ³ is returned to the calling program.                       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AA350-LOOKUP    SECTION.
       AA350.
             PERFORM SAVE-SCREEN.
             MOVE 4              TO W44-FUNCTION.
             PERFORM SCREEN-CONTENTS.
             CALL PRG-GLED-LUP USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY DATA-FILE-IDS W15-GLEDGER
                  ON EXCEPTION GO TO AA355
             END-CALL.
             CANCEL PRG-GLED-LUP.
             GO TO AA358.

       AA355.
             MOVE SPACE              TO WS-ERR-MES.
             STRING "Program- " DELIMITED SIZE PRG-GLED-LUP DELIMITED " " " not on disk, press ANY key" DELIMITED SIZE INTO WS-ERR-MES.
             PERFORM ERROR-MESSAGE.

       AA358.
             PERFORM RESTORE-SCREEN.

       AA399.
             EXIT.

       COPY "AA900.ALM".

      *
      *    ****    R E A D   F I L E S   R O U T I N E S
      *
       AB000           SECTION.

       COPY "CONTROL.RD".

       COPY "PARAM.RD".

       COPY "GACCNT.RD".

       COPY "GLTRAN.RD".

       COPY "GLJRN.RD".

       COPY "LEDTRN.RD".

       COPY "SHARED.RD".

       COPY "TXTRAN.RD".

      *                                                                 
      *     R E W R I T E   &   W R I T E   F I L E S   R O U T I N E S 
      *                                                                 
       AC000           SECTION.

       COPY "CONTROL.WR".

       COPY "PARAM.WR".

       COPY "GACCNT.WR".

       COPY "GLTRAN.WR".

       COPY "GLJRN.WR".

       COPY "LEDTRN.WR".

       COPY "TXTRAN.WR".

      /
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
      *
      *   ****    CALCULATE THE PERIOD INTO WHICH THIS DATE FALLS
      *
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

       COPY "GACCNT.LUP".

      /
      *    THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *
       AY000           SECTION.
       AY01.
             MOVE 01 TO REC-FILE.
             MOVE WS-PARKEY TO REC-KEY.
            PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-RECORD1 TO REC-PARAM.
             GO TO AY50.
       AY12.
            MOVE 12   TO REC-FILE.
            MOVE ZERO   TO REC-KEY REC-TYPE.
           IF WS-SKIP = "N"
               PERFORM READ-GACCNT-LOCK THRU READ-GACCNT-EXIT
           ELSE
               MOVE "N"          TO WS-SKIP.
             MOVE G-REC          TO REC-GACCNT.
             GO TO AY50.
       AY13.
             MOVE 13 TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE.
             MOVE T-REC          TO REC-GLTRAN.
             GO TO AY50.
       AY19.
             MOVE 19             TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE.
             MOVE TAX-RECORD1    TO REC-TXTRAN.
             GO TO AY50.
       AY39.
             MOVE 39             TO REC-FILE.
             MOVE WS-NETKEY      TO REC-KEY.
             MOVE NET-RECORD     TO REC-NETWORK.
             GO TO AY50.
       AY40.
             MOVE 99             TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
             MOVE SPACES         TO REC-DETAIL.
             PERFORM AY50.
             ADD 1               TO WS-TRANS.
           IF WS-RECOVER > 95
               CLOSE RECOVER
               OPEN OUTPUT RECOVER
               CLOSE RECOVER
               OPEN I-O RECOVER
               MOVE ZERO         TO WS-RECOVER.
             GO TO AY59.
       AY50.
             ADD 1               TO WS-RECOVER.
             MOVE WS-RECOVER     TO WS-RECKEY.
             MOVE WS-TRANS       TO REC-TRAN.
             WRITE REC-RECORD.
           IF WS-STATUS NOT = "00"
               DISPLAY "Write error Recovery file - Status " AT 2312
                        WITH FOREGROUND-COLOR 14
                        WS-STATUS WITH FOREGROUND-COLOR 15
               STOP RUN.
       AY59.
             EXIT.
      *
      *    ****    Start processing transaction
      *
       AY60.
    IF WS-SKIP = "Y" OR "A"
               GO TO AY999.
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
            MOVE "GL"   TO PAR-PROG(WS-USUB).
            MOVE LS-USER  TO PAR-USR(WS-USUB).
             PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      *
      *    ****    Read the LEDGER control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *
             MOVE 4              TO WS-NETKEY.
            PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
             PERFORM AY39 THRU AY59.
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
            PERFORM AY40 THRU AY59.
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
       HA000      SECTION 5.
       HA00.
             PERFORM ERASE-SCREEN.
             DISPLAY S11.
            MOVE ZERO   TO W15-ACCOUNT.
       HA05.
      DISPLAY "F1" AT 5002
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
       " to do Account Lookup - "
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
       "Esc" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
       "ape to exit"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
             ACCEPT S17 AT 0532.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE ZERO  TO W15-ACCOUNT
       GO TO HA999
   WHEN F1-KEY
       PERFORM AA350
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO HA05
        END-EVALUATE
        DISPLAY S17 AT 0532
               IF W15-ACCOUNT = ZERO
           GO TO HA05.
      PERFORM CLEAR-L50.
           IF W15-ACCOUNT = ZERO
               GO TO HA999.
            MOVE W15-ACCOUNT  TO G-AC.
             PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
            PERFORM CLEAR-L50.
          IF WS-F-ERROR = 8
              MOVE WS-ER6  TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               GO TO HA05.
             DISPLAY G-NAME AT 0632 WITH FOREGROUND-COLOR 11.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO HA00.
          IF G-BAL NOT = ZERO
              MOVE "May not delete"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               GO TO HA00.
      PERFORM HA20 THRU HA30.

       HA15.
             INITIALIZE G-REC.
      SUBTRACT 1   FROM G-ACCOUNTS.
             PERFORM REWRITE-GACCNT THRU WRITE-GACCNT-EXIT.
             PERFORM AY70 THRU AY999.
             GO TO HA00.

       HA20.
             PERFORM AY60 THRU AY999.
             PERFORM AY12 THRU AY59.
      DISPLAY "Deleting Transactions" AT 1412.
      INITIALIZE T-REC.
      MOVE G-AC   TO T-AC.
      PERFORM START-AT-GLTRAN-KEY THRU READ-GLTRAN-EXIT.
    IF WS-F-ERROR = 11
        GO TO HA30.
       HA25.
      PERFORM READ-GLTRAN-NEXT THRU READ-GLTRAN-EXIT.
    IF WS-F-ERROR = 11
        GO TO HA30.
    IF G-AC NOT = T-AC
        GO TO HA30.
      DISPLAY T-KEY AT 1434 WITH FOREGROUND-COLOR 6 HIGHLIGHT.
             MOVE 2              TO WS-ACTION.
      PERFORM AY13 THRU AY59.
      PERFORM DELETE-GLTRAN-REC THRU WRITE-GLTRAN-EXIT.
      GO TO HA25.
       HA30.
      EXIT.

       HA999.
             EXIT.

      /
       ZA000      SECTION 9.
       ZA00.
      MOVE LS-PARID  TO WS-PARID.
      MOVE LS-L-OR-N  TO W02-L-OR-N.
      MOVE WS-SYS-ID  TO W02-SYSID.
      PERFORM ZA55 THRU ZA60.
            PERFORM ERASE-SCREEN.
      *
      *    ****    D O  N O T  D I S P L A Y  E R R O R
      *     M E S S A G E S
      *
      MOVE 44   TO USER-NUMBER.
      MOVE 1   TO USER-SETTING.
      CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
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
           ELSE
           IF TRM-STDP (WS-S6) = 2
               MOVE "LPT2"       TO W02-PRINTER
           ELSE
           IF TRM-STDP (WS-S6) = 3
               MOVE "LPT3"       TO W02-PRINTER
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
       ZA02.
            OPEN OUTPUT RECOVER GLJRN.
             CLOSE RECOVER GLJRN.
             OPEN I-O RECOVER GLJRN.
            MOVE 1   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
            MOVE PAR-COMPANY  TO W90-CNAME.
            MOVE PAR-DMY  TO W05-TODAY.
            MOVE PAR-YMD  TO W05-TYMD.
      MOVE 6   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-DEBGL      TO W70-DEBGL.
             MOVE PAR-CREGL      TO W70-CREGL.
             MOVE PAR-INTGL      TO W70-INTGL.
             MOVE PAR-BNKGL      TO W70-BNKGL.
             MOVE PAR-UNPROF     TO W70-UNPROF.
             MOVE 4              TO WS-NETKEY.
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
      MOVE G-OPEN  TO WS-OPEN.
            ADD 1   TO WS-S2.
       ZA15.
           IF WS-S2 > 12
              SUBTRACT 12  FROM WS-S2.
            MOVE W30-MONTH(WS-S2)
     TO W35-MONTH(WS-S1).
           IF WS-S1 < 12
              ADD 1   TO WS-S1 WS-S2
               GO TO ZA15.
            MOVE ZERO   TO WS-ERROR.
           IF W70-BNKGL = ZERO
              DISPLAY "Parameters not set up - " AT 2020
         "ENTER" WITH FOREGROUND-COLOR 14
               ACCEPT WS-OPTION AT 2050 WITH FOREGROUND-COLOR 15 AUTO
              MOVE 1   TO WS-ERROR.
            MOVE WS-Y-END  TO WS-S1.
            MOVE W05-CY  TO WS-S2.
           IF W05-MM < WS-Y-END
              SUBTRACT 1  FROM WS-S2.
       ZA20.
            ADD 1   TO WS-S1.
           IF WS-S1 > 12
              MOVE 1   TO WS-S1
              ADD 1   TO WS-S2.
            MOVE WS-S2   TO W40-YEAR (WS-S1).
           IF WS-S1 NOT = WS-Y-END
               GO TO ZA20.
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
       ZA55.
            MOVE 1   TO WS-S1.
            MOVE SPACES  TO WS-MID-LNE.
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
            DISPLAY "Files locked - Try later" AT 5002
              WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
       " " WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
       WS-STATUS
       WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15
             " Press any key"
              WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
            ACCEPT WS-OPTION AT 5045
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
               MOVE W02-NETWORK TO WS-FILE
               MOVE WS-NETKEY TO WS-KEY
           ELSE
           IF WS-F-ERROR = 8
              MOVE W02-GACCNT  TO WS-FILE
              MOVE ZERO  TO WS-KEY
        MOVE G-AC  TO WS-KEYX
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
           IF WS-F-ERROR = 18
              MOVE W02-RECOVER  TO WS-FILE
              MOVE WS-RECKEY  TO WS-KEY
           ELSE
           IF WS-F-ERROR = 26
              MOVE W02-GLJRN  TO WS-FILE
              MOVE WS-GLJKEY  TO WS-KEY
           ELSE
           IF WS-F-ERROR = 32
               MOVE W02-TXTRAN   TO WS-FILE
               MOVE ZERO         TO WS-KEY
        MOVE TAX-KEY  TO WS-KEYX
          ELSE
          IF WS-F-ERROR = 37
              MOVE W02-SHARED  TO WS-FILE
              MOVE WS-SHARED  TO WS-KEY
           ELSE
           IF WS-F-ERROR = 39
               MOVE W02-LEDTRN   TO WS-FILE
               MOVE WS-LTRKEY    TO WS-KEY.
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
12.
             STOP RUN.
