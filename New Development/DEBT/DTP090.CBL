      $SET LINKCOUNT"448" GNT"DTP090.GNT"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******     ****     ******     ****     *
      *   **    **     **     **    **   **  **   **    **   **  **    *
      *   **    **     **     **    **  **    **  **    **  **    **   *
      *   **    **     **     *******   **    **   *******  **    **   *
      *   **    **     **     **        **    **        **  **    **   *
      *   **    **     **     **         **  **   **    **   **  **    *
      *   *******      **     **          ****     ******     ****     *
      *                                                                *
      *       ENGLISH                                                  *
      *                                                                *
      *     D E B T O R   R E C A L C U L A T E   P U R C H A S E S    *
      *                                                                *
      *       Version 9.00.70 - March 2015                             *
      *                                                                *
      ******************************************************************
      *                                                                *
      *  May 2006 - New field included in Accounts Receivable          *
      *             for recording number of outstanding Jobs           *
      *             linked to the account. Additional filler           *
      *             added for future expansion.                        *
      *                                                                *
      *  Jan 2008 - New file (DEBALT)- Debtor alternate index included *
      *             for lookups, using any word contained in the       *
      *             Debtor name.                                       *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     DTP090.
       AUTHOR.         J W LEMMON (APAC).
       DATE-WRITTEN.   OCTOBER 1982.

                COPYRIGHT NOTICE: COPYRIGHT (C) 1982 - 2016
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

      *COPY "DBTRAN.SL".

       COPY "DEBTOR.SL".

       COPY "INVOIC.SL".

       COPY "PARAM.SL".

       COPY "SHARED.SL".

       DATA DIVISION.
       FILE SECTION.

       COPY "CONTROL.FDE".

      *COPY "DBTRAN.FDE".

       COPY "DEBTOR.FDE".

       COPY "INVOIC.FDE".

       COPY "PARAM.FDE".

       COPY "SHARED.FDE".

      /
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
       77  WS-SUB    PIC 9(04)  COMP-5.
       77  WS-S            PIC  9(04)    COMP-5.
       77  WS-S1           PIC  9(04)    COMP-5.
       77  WS-S2           PIC  9(04)    COMP-5.
       77  WS-S3           PIC  9(04)    COMP-5.
       77  WS-S4           PIC  9(04)    COMP-5.
       77  WS-S5           PIC  9(04)    COMP-5.
       77  WS-S6           PIC  9(04)    COMP-5.
       77  WS-S7           PIC  9(04)    COMP-5.
       77  WS-S8           PIC  9(04)    COMP-5.
       77  WS-S9           PIC  9(04)    COMP-5.
       77  WS-IXS1         PIC  9(04)    COMP-5.
       77  WS-IXS2         PIC  9(04)    COMP-5.
       77  WS-IXS3         PIC  9(04)    COMP-5.
       77  WS-IXS4         PIC  9(04)    COMP-5.
       77  WS-DBTKEY       PIC  9(06)    COMP-5.
       77  WS-SHARED    PIC 9(04)  COMP-5 VALUE 1.
       77  WS-ENQPOS       PIC  9(06)    COMP-5.
       77  WS-ENQEND       PIC  9(06)    COMP-5.
       77  WS-NETKEY       PIC  9(06)    COMP-5.
       77  WS-PARKEY       PIC  9(06)    COMP-5.
       77  WS-RECKEY       PIC  9(06)    COMP-5.
       77  WS-RECOVER      PIC  9(06)    COMP-5.
       77  WS-TRANS        PIC  9(04)    COMP-5 VALUE 1.
       77  WS-FIN-RATE    PIC S9(03)V99 COMP-3.
       77  WS-PERIOD       PIC  9(02).
       77  WS-NTP          PIC  X(01).
       77  WS-KEYSTR    PIC 9(06)  COMP-5.
       77  WS-OPTION       PIC  X(01).
       77  WS-SKIP         PIC  X(01) VALUE "N".
       77  WS-TAX          PIC  X(01).
       77  WS-TYPE         PIC  X(01).
       77  WS-ERROR        PIC  9(01) VALUE ZERO.
       77  WS-EKEY         PIC  9(06)    COMP-5.
       77  WS-CHAR1        PIC  X(01).
       77  WS-CHAR2        PIC  X(01).
       77  WS-COMP         PIC  9(01).
       77  WS-BATCH        PIC  9(06) COMP-5.
       77  TODAY-DDMMYY    PIC 9(08) COMP-5.
       77  WS-USUB    PIC 9(04) COMP-5.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Used by calling program (WORKING-STORAGE) and called program ³
      *    ³ (LINKAGE SECTION) for Main screen layout and headings.   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       01  CRT-HEADINGS.
      *
      *    TYPE:
      *      C = Clear comment line (50)
      *      E = Clear lines (any line or lines between 5 and 46)
      *       H = Change heading
      *       S = Clear the screen and display headings
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "DTP090".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "ACCOUNTS RECEIVABLE - RECALC PURCHASES".
      *
      *    Company Name
      *
           03  CRT-COMPANY         PIC  X(40).
      *
      *    WorkStation / Supervisor
      *
           03  CRT-WRKHD           PIC  X(11).
           03  CRT-WRKST           PIC  X(03).

      /
       01  WS-DB-LINE.
           03  WS-TOP-LNE.
               05  WS-TCHR PIC X(01) OCCURS 80.
           03  WS-T-LINE REDEFINES WS-TOP-LNE.
               05  FILLER  PIC X(01).
               05  WS-H-LINE
                           PIC X(78).
               05  FILLER  PIC X(01).
           03  WS-TOP-LNE2.
               05  WS-TCH  PIC X(01) OCCURS 80.
           03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
               05  FILLER      PIC  X(01).
               05  WS-TOP-COMP PIC  X(40).
              05  FILLER      PIC  X(23).
        05  WS-WRKHD    PIC  X(11).
              05  FILLER      PIC  X(01).
        05  WS-WRKST    PIC  X(03).
              05  FILLER      PIC  X(01).
          03  WS-MID-LNE.
               05  WS-MCHR PIC X(01) OCCURS 80.
    03  WS-MDDT-LNE REDEFINES WS-MID-LNE.
        05  WS-MDT1     PIC  X(01).
        05  WS-MDT78    PIC  X(78).
        05  WS-MDT80    PIC  X(01).
          03  WS-MID-LNE2.
        05  FILLER      PIC  X(01) VALUE "³".
        05  WS-BLNK78   PIC  X(78) VALUE ALL "°".
        05  FILLER      PIC  X(01) VALUE "³".
          03  WS-BOT-LNE.
               05  WS-BCHR PIC X(01) OCCURS 80.
           03  WS-B-LINE REDEFINES WS-BOT-LNE.
               05  FILLER  PIC X(01).
               05  WS-F-LINE
                           PIC X(78).
               05  FILLER  PIC X(01).
           03  WS-BOT-LNE2.
               05  WS-BCH  PIC X(01) OCCURS 80.
           03  WS-B-LINE REDEFINES WS-BOT-LNE2.
               05  FILLER  PIC X(01).
               05  WS-MEM-LINE.
                  07  WS-SRT-LINE
                           PIC X(50).
                  07  FILLER
                           PIC X(10).
               05  FILLER  PIC X(19).
           03  WS-MM-LINE.
               05  FILLER  PIC  X(01) VALUE X"B3".
               05  FILLER  PIC  X(60) VALUE SPACES.
               05  FILLER  PIC  X(01) VALUE X"B3".
           03  WS-HME-LINE PIC  X(60) VALUE ALL X"CD".

       COPY "WS.WS".

       01  WS-PARID.
          03  WS-SYS-ID       PIC  X(03).

       01  W10-REF.
           03  W10-NUM         PIC  9(08).
          03  W10-DATE.
        05  W10-EDTE  PIC  99/99/9999.

      *
      *    Date routines for accepting checking and formatting have
      *    been removed from each program and the DateCheck program
      *    will be called to handle these routines.
      *
      *    The following copy 'DateVariables' now includes the 'W12.WS'
      *    and 'W20.WS' variables and is passed to DateCheck in the CALL
      *    statement.
      *
       COPY DateVariables.

       COPY "W15.WS".

       01  W25-CALCS.
           03  W25-RESULT      PIC 9(05)V99.
           03  W25-RESULT1 REDEFINES W25-RESULT.
               05  W25-WHOLE   PIC 9(05).
               05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY    PIC 9(04).
               05  W25-SUB    PIC 9(01).
               05  FILLER     PIC 9(02).
           03  W25-TOTAL       PIC S9(09)V99 COMP-3.
           03  W25-VALUE       PIC S9(09)V99 COMP-3.

       01  W30-AGE-DATES.
    03  W30-120        PIC  9(08) COMP-3.
    03  W30-90        PIC  9(08) COMP-3.
    03  W30-60        PIC  9(08) COMP-3.
    03  W30-30        PIC  9(08) COMP-3.
    03  W30-CUR        PIC  9(08) COMP-3.

       COPY "W40.DBT".

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       01  W60-NME-ADD.
           03  W60-NAME.
               05  W60-NCHAR OCCURS 40 PIC X(01).
           03  W60-ADDRESS.
               05  W60-ACHAR OCCURS 70 PIC X(01).
           03  W60-ADDR REDEFINES W60-ADDRESS.
               05  W60-ADD     PIC X(60).
               05  FILLER      PIC X(10).
           03  W60-ADDRESS1.
               05  W60-PAD     PIC X(60).
               05  FILLER      PIC X(10).

       01  W75-KEYS.
           03  W75-S          PIC 9(02) COMP-5.
           03  W75-S1         PIC 9(02) COMP-5.
           03  W75-KEY        PIC  9(06)    COMP-5.
           03  W75-ALLOCATED  PIC S9(09)V99 COMP-3.
           03  W75-BALANCE    PIC S9(09)V99 COMP-3.
           03  W75-DNO        PIC X(06) OCCURS 18.

       01  W75-TITLES.
           03  W75-DESCRIP.
               05  FILLER      PIC X(28)      VALUE
                           "Mr     Mnr    Mrs    Mev    ".
               05  FILLER      PIC X(28)      VALUE
                           "Miss   Mej    Ms     Ms     ".
               05  FILLER      PIC X(28)      VALUE
                           "Dr     Dr     Rev    Ds     ".
               05  FILLER      PIC X(28)      VALUE
                           "Prof   Prof   The HonSy Edel".
               05  FILLER      PIC X(28)      VALUE
                           "                            ".
           03  W75-CODES REDEFINES W75-DESCRIP.
               05  W75-TITLE  OCCURS 10.
                   07  W75-ETITLE  PIC X(07).
                   07  W75-ATITLE  PIC X(07).

       01  W80-ACCOUNT.
           03  W80-ACNO        PIC X(06).
           03  W80-TYPE        PIC 9(01).

       01  W85-PASS.
           03  W85-SUPER       PIC  X(06)    OCCURS 9.
           03  W85-ENTRY       PIC  9(02)    COMP-3.
           03  W85-MARG        PIC S9(02)V99.

       01  W90-TRAN.
           03  W90-120         PIC S9(09)V99 COMP-3.
           03  W90-90          PIC S9(09)V99 COMP-3.
           03  W90-60          PIC S9(09)V99 COMP-3.
           03  W90-30          PIC S9(09)V99 COMP-3.
           03  W90-CUR         PIC S9(09)V99 COMP-3.
           03  W90-BAL         PIC S9(09)V99 COMP-3.
           03  W90-IDTE        PIC 9(06)     COMP-3.
           03  W90-INT         PIC S9(09)V99 COMP-3.

       01  W95-STM.
           03  W95-COMP        PIC X(40).

       01  W100-EDIT.
           03  W100-LBAL       PIC Z(06)9.99-.
           03  W100-LARR       PIC Z(06)9.99.
           03  W100-BAL        PIC Z(08)9.99-.
           03  W100-CUR        PIC Z(08)9.99-.
           03  W100-30         PIC Z(08)9.99-.
           03  W100-60         PIC Z(08)9.99-.
           03  W100-90         PIC Z(08)9.99-.
           03  W100-120        PIC Z(08)9.99-.
           03  W100-INT        PIC Z(08)9.99.
           03  W100-CRL        PIC Z(06)9.
           03  W100-KEY        PIC Z(05)9.

       LINKAGE SECTION.

       COPY "CHAIN.LS".

       COPY "FILE.IDS".

       SCREEN SECTION.

       01  S05A.
    03  LINE  7 COLUMN 44 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC 99/99/9999 USING W15-START AUTO.

       01  S05D.
    03  LINE  4 COLUMN 24 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown VALUE
    "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
    03  LINE  5 COLUMN 24 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown VALUE
    "³   Include transactions from:   ³".
    03  LINE  6 COLUMN 24 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown VALUE
    "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".
    03  LINE  7 COLUMN 24 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown VALUE
    "³   Starting Date :              ³".
    03  LINE  8 COLUMN 24 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown VALUE
    "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".
    03  LINE  9 COLUMN 24 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown VALUE
    "³Dates to be keyed in as ".
    03        COLUMN 49 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown VALUE "DDMMCCYY".
    03        COLUMN 57 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown VALUE "³".
    03  LINE 10 COLUMN 24 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown VALUE
    "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

      /
       PROCEDURE DIVISION USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY DATA-FILE-IDS.
       AA000         SECTION.
       AA00.
          IF LS0-DBLEV < 9
        MOVE "Not Authorised"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO AA49.
            PERFORM ZA000-INIT.
      PERFORM CM000.

       AA49.
             EXIT PROGRAM.

      /    *************************************************************
      *    ****    ROUTINES TO HANDLE VARIOUS FUNCTIONS FOR THE
      *     S C R E E N ,   K E Y B O A R D   &  M O U S E
      *    *************************************************************
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³    SAVE-SCREEN /-2/-3  and  RESTORE-SCREEN /-2/-3    ³
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ³                      SCREEN-SHADOW                        ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ To routine is used to display a shadow down the right and ³
      *    ³ along the bottom of a pop-up box. The parameters that are ³
      *    ³ required:                                                 ³
      *    ³          SHADE-ROW   - Top line of the box + 1.           ³
      *    ³          SHADE-COL   - Left line of box + 2.              ³
      *    ³          SHADE-WIDTH - Width of the box - 2.              ³
      *    ³          SHADE-LINES - Hight of box - 1.                  ³
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ³                      CHECK-CORRECT                        ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ This routine displays a pop-up window with the message    ³
      *    ³           "Press Y if correct - N if incorrect"           ³
      *    ³                                                           ³
      *    ³ The response is returned in WS-OPTION (in upper case).    ³
      *    ³                                                           ³
      *    ³ To start with the pop-up window higher or lower than row  ³
      *    ³ 13 (default); move another value to SLIN and PERFORM      ³
      *    ³ CHECK-SETUP THRU CHECK-EXIT.                              ³
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ****   T H I S   R O U T I N E   I S   U S E D   T O
      *         D I S P L A Y   E R R O R   M E S S A G E S
      *    *************************************************************
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                      ERROR-MESSAGE                        ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ To display the error message higher or lower (default is  ³
      *    ³ line 20) Move the line number which must be used as the   ³
      *    ³ top line to WS-MES-LINE. The message to be displayed must ³
      *    ³ be in WS-ERR-MES. PERFORM ERROR-MESSAGE.                  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "FUNCTION.CRT".

       COPY "LOCKED.REC".

       COPY "CLEAR.CRT".

       COPY "DATE.CHK".

      /
      *    ****    READ FILES ROUTINES
      *
       AC000              SECTION.

       COPY "CONTROL.RD".

      *COPY "DBTRAN.RD1".

       COPY "DEBTOR.RD".

       COPY "INVOIC.RD".

       COPY "PARAM.RD".

       COPY "SHARED.RD".

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
       AD000             SECTION.

       COPY "CONTROL.WR".

       COPY "DEBTOR.WR".

       COPY "PARAM.WR".

      /
      *    THIS ROUTINE LOCKS THE CONTROL FILE DEBTOR RECORD
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
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Red HIGHLIGHT.
            COMMIT.
      ACCEPT WS-STIME FROM TIME.
      MOVE 400   TO WS-WAIT.
            PERFORM LOCK-REC-LOOP.
            DISPLAY SPACE AT 5051
       WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Cyan.
             GO TO AY60.

       AY61.
            MOVE "DB"   TO PAR-PROG(WS-USUB).
            MOVE LS-USER  TO PAR-USR(WS-USUB).
             PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      *
      *    ****    Read the DEBTOR control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *
            MOVE 1   TO WS-NETKEY.
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
           IF PAR-PROG(WS-SUB) = "DB" OR "DS"
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
   WITH BACKGROUND-COLOR Cyan
        FOREGROUND-COLOR Brown HIGHLIGHT BLINK
        ACCEPT WS-STIME FROM TIME
        PERFORM LOCK-REC-LOOP
        DISPLAY "Waiting" AT 5072
   WITH BACKGROUND-COLOR Cyan
        FOREGROUND-COLOR Brown HIGHLIGHT BLINK
        GO TO AY60.
            DISPLAY SPACE AT 5072
       WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Cyan.

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
        IF WS-USUB < 24
     ADD 1  TO WS-USUB
     GO TO AY72
        ELSE
     GO TO AY85.

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

       AY85.
            COMMIT.

       AY999.
             EXIT.
      /
      *    ****    R E - C A L C U L A T E   B A L A N C E S
      *
       CM000         SECTION 5.
       CM00.
             MOVE "S"                TO CRT-TYPE.
             CALL "UTP\CRTHEAD" USING CRT-HEADINGS LS-USER-ID.
             CALL "UTP\NORECOV" USING WS-OPTION.

       CM05.
           IF WS-OPTION = "N"
               GO TO CM999.
      PERFORM SAVE-SCREEN-3.
      MOVE 5   TO SHADE-ROW.
      MOVE 26   TO SHADE-COL.
      MOVE 32   TO SHADE-WIDTH.
      MOVE 6   TO SHADE-LINES.
      DISPLAY S05D.
      PERFORM SCREEN-SHADOW.
      MOVE W12-TODAY  TO W15-START.
      *
      *    ****    S E T   S T A R T   D A T E
      *
      MOVE 1   TO W15-SDAY.
    IF W15-SMNTH = 12
        MOVE  1   TO W15-SMNTH
    ELSE
        ADD 1   TO W15-SMNTH
        SUBTRACT 1  FROM W15-SCCYY.
      MOVE W15-SDAY  TO W15-EDD.
      MOVE W15-SMNTH  TO W15-EMM.
      MOVE W15-SCENT  TO W15-ECC.
      MOVE W15-SYEAR  TO W15-EYY.
      DISPLAY "Press " AT 5052
        WITH FOREGROUND-COLOR Blue BACKGROUND-COLOR Cyan
       "Esc" WITH FOREGROUND-COLOR Brown HIGHLIGHT
    BACKGROUND-COLOR Cyan
       "ape to Exit"
        WITH FOREGROUND-COLOR Blue BACKGROUND-COLOR Cyan.

       CM10.
      ACCEPT S05A.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO CM999
   WHEN OTHER  CALL X"E5"
       GO TO CM10
        END-EVALUATE.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
        GO TO CM10.
      PERFORM CLEAR-L50.
      MOVE W15-START  TO W10-EDTE.
             PERFORM CHECK-DATE.
           IF WS-ERROR = 1
              MOVE "Invalid date" TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO CM10.
      MOVE W20-DTE  TO W15-S-YMD.
    IF W15-E-YMD > W15-S-YMD
              MOVE "Date may not be prior to 11 months ago"
     TO WS-ERR-MES
              PERFORM ERROR-LENGTH THRU ERROR-EXIT
        GO TO CM10.
      PERFORM RESTORE-SCREEN-3.
            MOVE 1   TO WS-S3.
             DISPLAY "Account :" AT 1812.
             PERFORM AY60 THRU AY999.
      MOVE "     !"  TO DEB-ACNO.
             PERFORM START-AT-DEBT-AC THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 6
               GO TO CM85.

       CM20.
             PERFORM READ-DEBTOR-NEXT-LOCK THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 6
               GO TO CM85.
            DISPLAY DEB-ACNO AT 1822
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
      MOVE 1   TO WS-S1.

       CM25.
            MOVE ZERO   TO DEB-MTHPUR(WS-S1).
    IF WS-S1 < 12
        ADD 1   TO WS-S1
        GO TO CM25.
            MOVE DEB-ACNO  TO DOC-AC W80-ACNO.
      MOVE " 0000000"  TO DOC-REF.
      MOVE ZERO   TO DOC-TYPE.
             PERFORM START-AT-INVOIC-AC THRU READ-INVOIC-EXIT.
           IF WS-F-ERROR NOT = ZERO
        GO TO CM80.

       CM70.
            PERFORM READ-INVOIC-NEXT THRU READ-INVOIC-EXIT.
          IF (WS-F-ERROR NOT = ZERO) OR
              (DOC-AC NOT = W80-ACNO)
              GO TO CM80.
          IF DOC-TYPE NOT = 0
              GO TO CM70.
    IF DOC-DTE < W15-E-YMD
        GO TO CM70.
      MOVE DOC-DTE  TO W21-DATE.
      ADD DOC-VALUE  TO DEB-MTHPUR(W21-MM).
      GO TO CM70.

       CM80.
      PERFORM REWRITE-DEBTOR-UNLOCK THRU WRITE-DEBTOR-EXIT.
             GO TO CM20.

       CM85.
      PERFORM AY70 THRU AY999.

       CM999.
             EXIT.
      /
      *    ****    I N I T I A L I Z E   P R O G R A M
      *
       ZA000-INIT        SECTION 8.
       ZA000-OPEN.
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

            MOVE 1   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-COMPANY    TO W95-COMP.
             MOVE PAR-DMY        TO W12-TODAY.
             MOVE PAR-YMD        TO W12-T-YMD.
      *
      *    ****    S E T   U P   A G E   D A T E S
      *
             MOVE W12-T-YMD      TO W22-DTE2.
      *    IF W22-DD2 > 27
      *        ADD 1   TO W22-MM2.
       ZA47.
           IF W22-MM2 > 01
               SUBTRACT 1        FROM W22-MM2
           ELSE
               MOVE 12           TO W22-MM2
        IF W22-YY2 > ZERO
     SUBTRACT 1  FROM W22-YY2
        ELSE
     SUBTRACT 1  FROM W22-CC2
     MOVE 99  TO W22-YY2.
    IF W22-MM2 = 02
               MOVE 28           TO W22-DD2
           ELSE
           IF W22-MM2 = 04 OR 06 OR 09 OR 11
               MOVE 30           TO W22-DD2
           ELSE
               MOVE 31           TO W22-DD2.
       ZA48.
           IF W12-MM = W22-MM2
               IF W12-DD < W22-DD2
                   MOVE W12-DD   TO W22-DD2.
       MOVE W22-DTE2   TO W30-CUR.
      PERFORM ZA47.
             MOVE W22-DTE2       TO W30-30.
      PERFORM ZA47.
             MOVE W22-DTE2       TO W30-60.
      PERFORM ZA47.
             MOVE W22-DTE2       TO W30-90.
      PERFORM ZA47.
             MOVE W22-DTE2       TO W30-120.
            GO TO ZA999-EXIT.
       ZA55.
             MOVE 1              TO WS-S1.
            MOVE SPACES  TO WS-MID-LNE.
       ZA60.
             MOVE WS-G1          TO WS-TCHR (WS-S1) WS-BCHR (WS-S1).
             MOVE WS-G8          TO WS-TCH  (WS-S1) WS-BCH  (WS-S1).
           IF WS-S1 < 80
               ADD 1             TO WS-S1
               GO TO ZA60.
             MOVE WS-G9          TO WS-TCH  (1).
             MOVE WS-G10         TO WS-TCH  (80).
             MOVE WS-G11         TO WS-BCH  (1).
             MOVE WS-G12         TO WS-BCH  (80).
             MOVE WS-G14         TO WS-TCHR (1)  WS-BCHR (1).
             MOVE WS-G13         TO WS-TCHR (80) WS-BCHR (80).
             MOVE WS-G2          TO WS-TCHR (8)  WS-TCHR (49)
                                    WS-TCHR (61) WS-TCHR (72).
             MOVE WS-G3          TO WS-MCHR (8)  WS-MCHR (49)
                                    WS-MCHR (61) WS-MCHR (72)
              WS-MCHR (1)  WS-MCHR (80).
             MOVE WS-G4          TO WS-BCHR (8)  WS-BCHR (49)
                                    WS-BCHR (61) WS-BCHR (72).
             MOVE WS-G13         TO WS-TCHR (80) WS-BCHR (80).
             MOVE WS-G14         TO WS-TCHR (1)  WS-BCHR (1).
      MOVE LS-COMPANY  TO WS-TOP-COMP.
    IF LS-USER = LS-SYS-ID
        MOVE "SupervisorÄ"  TO WS-WRKHD
    ELSE
        MOVE "Workstation"  TO WS-WRKHD
        MOVE LS-USER    TO WS-WRKST.
       ZA999-EXIT.
             EXIT.
      /
      *    ****    I / O   E R R O R   M E S S A G E S
      *
       ZB000-ERROR      SECTION 8.

       OPEN-ERROR.
             PERFORM ERASE-SCREEN.
             DISPLAY "Open error" AT 0812 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             GO TO DISPLAY-FILE-NAME.

       READ-ERROR.
             PERFORM ERASE-SCREEN.
             DISPLAY "Read error" AT 0812 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             GO TO DISPLAY-FILE-NAME.

       WRITE-ERROR.
             PERFORM ERASE-SCREEN.
             DISPLAY "Write error" AT 0812 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             GO TO DISPLAY-FILE-NAME.
       I-O-ERROR-EXIT.
             EXIT.

       DISPLAY-FILE-NAME.
           IF WS-F-ERROR = 2
              MOVE W02-NETWORK  TO WS-FILE
              MOVE WS-NETKEY  TO WS-KEY
      *    ELSE
      *    IF WS-F-ERROR = 5
      *        MOVE W02-DBTRAN  TO WS-FILE
      *        MOVE ZERO  TO WS-KEY
      *        MOVE TRA-KEY  TO WS-KEYX
           ELSE
           IF WS-F-ERROR = 6
              MOVE W02-DEBTOR  TO WS-FILE
               MOVE ZERO         TO WS-KEY
               MOVE DEB-ACNO     TO WS-KEYX
          ELSE
           IF WS-F-ERROR = 12
               MOVE W02-INVOIC   TO WS-FILE
               MOVE ZERO         TO WS-KEY
               MOVE DOC-KEY      TO WS-KEYX
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
             DISPLAY "File - " AT 1012 WS-FILE WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
             DISPLAY "Status " AT 1212
                      WS-STATUS WITH FOREGROUND-COLOR Cyan HIGHLIGHT
                     ": " WS-STAT-MESSAGE WITH FOREGROUND-COLOR Brown HIGHLIGHT.
           IF WS-STATUS NOT = "94"
               DISPLAY "Key    " AT 1412
                        WS-KEY WITH FOREGROUND-COLOR Cyan HIGHLIGHT
                        " " WS-KEYX WITH FOREGROUND-COLOR Cyan HIGHLIGHT
               DISPLAY "Reverse backup or contact program Support"
                        AT 1612.
               DISPLAY "Please make a note of these details" AT 1812.
             STOP RUN.
