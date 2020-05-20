      $SET LINKCOUNT"384" GNT"DTP032.GNT"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******     ****     ******    ******    *
      *   **    **     **     **    **   **  **   **    **  **    **   *
      *   **    **     **     **    **  **    **        **       **    *
      *   **    **     **     *******   **    **     ****      **      *
      *   **    **     **     **        **    **        **   **        *
      *   **    **     **     **         **  **   **    **  **         *
      *   *******      **     **          ****     ******   ********   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     D E B T O R   C O N T R A C T   R E P O R T S              *
      *                                                                *
      *       Version 9.00.82 - April 2016                             *
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
       PROGRAM-ID.     DTP032.
       AUTHOR.         J W LEMMON (APAC).
       DATE-WRITTEN.   February 1985.

                COPYRIGHT NOTICE: COPYRIGHT (C) 1985 - 2016
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

       COPY "DEBTOR.SL".

       COPY "DBFOTO.SL".

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

       COPY "DEBTOR.FDE".

       COPY "DBFOTO.FDE".

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
       77  WS-CHECK               PIC  X(18)     VALUE "aeWlimemnomLalismJ".
       77  WS-SUB-1                PIC  9(04)     COMP-5.
       77  WS-SUB-2                PIC  9(04)     COMP-5.
       77  WS-SUB-3                PIC  9(04)     COMP-5.
       77  WS-SUB-4                PIC  9(04)     COMP-5.
       77  WS-SUB-5                PIC  9(04)     COMP-5.
       77  WS-SUB-6                PIC  9(04)     COMP-5.
       77  WS-IXS1                 PIC  9(04)     COMP-5.
       77  WS-IXS2                 PIC  9(04)     COMP-5.
       77  WS-IXS3                 PIC  9(04)     COMP-5.
       77  WS-IXS4                 PIC  9(04)     COMP-5.
       77  WS-MSUB                 PIC  9(04)     COMP-5.
       77  WS-PARKEY       PIC 9(04)     COMP-5.
       77  WS-DEPKEY       PIC 9(04)     COMP-5.
       77  WS-DEBKEY       PIC 9(06)     COMP-5.
       77  WS-NETKEY       PIC 9(06)     COMP-5.
       77  WS-WRKEY        PIC 9(04)     COMP-5.
       77  WS-GST-RATE     PIC S9(03)V99 COMP-3.
       77  WS-PRN-NO    PIC X(01)  COMP-X VALUE 9.
       77  WS-PRN-STAT    PIC X(01)  COMP-X.
       77  WS-NTP    PIC 9(01).
       77  WS-LINES        PIC 9(02).
       77  WS-PAGE         PIC 9(04)     COMP-5.
       77  WS-OPTION       PIC X(01).
       77  WS-AGE          PIC X(01).
       77  WS-SEQ          PIC X(01).
       77  WS-CREDIT       PIC X(01).
       77  WS-ZERO         PIC X(01).
       77  WS-NUM          PIC Z9.
       77  WS-ADDR         PIC X(01).
       77  WS-ERROR        PIC 9(01).
       77  WS-ERROR1       PIC 9(01).
       77  WS-PGE-LENGTH   PIC 9(02)  VALUE 66.
       77  WS-PRN-LENGTH   PIC 9(02)  VALUE 62.
       77  WS-EXPAND    PIC X(01)  VALUE " ".
       77  WS-COMP         PIC 9(01).
       77  WS-IND    PIC 9(01).
       77  WS-TYPE         PIC X(01).
       77  WS-HEAD         PIC X(01).
       77  WS-MESS         PIC X(02).
       77  WS-ACNO         PIC X(06).
       77  WS-S1           PIC 9(04)     COMP-5.
       77  WS-S2           PIC 9(04)     COMP-5.
       77  WS-S3           PIC 9(04)     COMP-5.
       77  WS-S4           PIC 9(04)     COMP-5.
       77  WS-S5           PIC 9(04)     COMP-5.
       77  WS-S6           PIC 9(04)     COMP-5.
       77  WS-S7           PIC 9(04)     COMP-5.
       77  WS-S8           PIC 9(04)     COMP-5.
       77  WS-AC           PIC X(07) VALUE "Account".
       77  PRG-DEBT-LUP            PIC  X(12)    VALUE "DTP\DTPLOOK".
       77  TODAY-DDMMYY    PIC 9(08) COMP-5.
       77  WS-USUB    PIC 9(04) COMP-5.
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

       01  WS-HELP.
    03  WS-MODULE       PIC  X(03) VALUE "DTP".
    03  WS-PROG        PIC  X(03) VALUE "001".

       01  WS-PARID.
          03  WS-SYS-ID       PIC  X(03).

       01  W02-PRINTER-DETAILS.
           03  W02-PRINTER         PIC  X(12).
           03  W02-PGE-LENGTH      PIC  9(03).
           03  W02-PRN-LENGTH      PIC  9(03).
           03  W02-LINAGE          PIC  9(03).
           03  W02-PRN-STATUS      PIC  X(01)    VALUE "C".
      *
      *    ****    D  =  Detail line
      *            C  =  Condensed print
      *            E  =  Expanded print
      *            H  =  Header line
      *            X  =  Cancel expanded print
      *            1  =  10 Characters per inch
      *            2  =  12 Characters per inch
      *            3  =  17 Characters per inch
      *            6  =  6 Lines per inch
      *            8  =  8 Lines per inch
      *
           03  W02-PRN-TYPE        PIC  X(01).
           03  W02-PRN-LINE        PIC  X(136).

           03  R-L1                                REDEFINES W02-PRN-LINE.
               05  R-DET           PIC  X(136).
           
           03  R-L2                                REDEFINES W02-PRN-LINE.
               05  R-DET2          PIC  X(80).
           
           03  REP-LINE3                           REDEFINES W02-PRN-LINE.
               05  REP-PAGE        PIC  X(06).
               05  REP-P-NO        PIC  Z(03)9.
               05  FILLER          PIC  X(24).
               05  REP-RESTART.
                   07  REP-COMPANY PIC  X(40).
                   07  REP-HEAD    PIC  X(24).
               05  FILLER          PIC  X(19).
               05  REP-DATE        PIC  X(06).
               05  REP-DTE         PIC  Z9/99/99.
           
           03  REP-LINE3A                          REDEFINES W02-PRN-LINE.
               05  REP-EXPP        PIC  X(04).
               05  REP-CO          PIC  X(40).
           
           03  REP-LINE4                           REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(11).
               05  REP-HEAD1       PIC  X(04).
               05  FILLER          PIC  X(06).
               05  REP-HEAD2       PIC  X(15).
               05  FILLER          PIC  X(06).
               05  REP-HEAD3       PIC  X(15).
               05  FILLER          PIC  X(06).
               05  REP-HEAD4       PIC  X(06).
               05  FILLER          PIC  X(11).
           
           03  REP-LINE5                           REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(12).
               05  REP-TRAN        PIC  9(02).
               05  FILLER          PIC  X(09).
               05  REP-ENG         PIC  X(12).
               05  FILLER          PIC  X(09).
               05  REP-AFR         PIC  X(12).
               05  FILLER          PIC  X(10).
               05  REP-ACT         PIC  X(01).
               05  FILLER          PIC  X(13).
           
           03  REP-LINE6                           REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(06).
               05  REP-HEAD5       PIC  X(07).
               05  REP-BATCH       PIC  Z(04)9.
               05  FILLER          PIC  X(04).
               05  REP-HEAD6       PIC  X(20).
               05  FILLER          PIC  X(38).
           
           03  REP-LINE7                           REDEFINES W02-PRN-LINE.
               05  REP-HEAD7       PIC  X(07).
               05  REP-HEAD8       PIC  X(39).
               05  REP-HEAD9       PIC  X(09).
               05  REP-HEAD10      PIC  X(30).
               05  REP-HEAD11      PIC  X(11).
               05  REP-HEAD12      PIC  X(05).
               05  REP-HEAD13      PIC  X(07).
               05  REP-HEAD14      PIC  X(07).
               05  REP-HEAD15      PIC  X(09).
               05  REP-HEAD16      PIC  X(10).
           
           03  REP-LINE8                           REDEFINES W02-PRN-LINE.
               05  REP-AC          PIC  X(06).
               05  FILLER          PIC  X(01).
               05  REP-NAME        PIC  X(38).
               05  FILLER          PIC  X(01).
               05  REP-CONT        PIC  X(08).
               05  FILLER          PIC  X(01).
               05  REP-DESC        PIC  X(29).
               05  FILLER          PIC  X(01).
               05  REP-INST        PIC  Z(06)9.99  BLANK ZERO.
               05  FILLER          PIC  X(01).
               05  REP-TYPE        PIC  X(04).
               05  FILLER          PIC  X(01).
               05  REP-FREQ        PIC  X(06).
               05  FILLER          PIC  X(01).
               05  REP-ESC         PIC  ZZ9.99     BLANK ZERO.
               05  FILLER          PIC  X(01).
               05  REP-ADATE       PIC  99/99/99   BLANK ZERO.
               05  FILLER          PIC  X(01).
               05  REP-RATE        PIC  Z(05)9.999 BLANK ZERO.
           
       01  W05-LIST.
           03  W05-OPT         PIC 9(03).
           03  W05-TYPES       REDEFINES W05-OPT.
               05  W05-COMP    PIC 9(01).
               05  W05-INDV    PIC 9(01).
               05  W05-INT     PIC 9(01).

      *
      *    Date routines for accepting checking and formatting have
      *    been removed from each program and the DateCheck program
      *    will be called to handle these routines.
      *
      *    The following copy 'DateVariables' now includes the 'W12.WS'
      *    and 'W20.WS' variables and is passed to DateCheck in the CALL
      *    statement.
      *
       COPY "DateVariables.cpy".

       COPY "W40.DBT".

       COPY "W40.WS".

       COPY "FUNCTION.WS".

      *COPY "W50.WS".

       01  W60-NME-ADD.
           03  W60-NAME.
               05  W60-NCHAR OCCURS 40 PIC X(01).
           03  W60-ADDRESS.
               05  W60-ACHAR OCCURS 60 PIC X(01).

       01  W75-KEYS.
           03  W75-S          PIC 9(02) COMP-5.
           03  W75-S1         PIC 9(02) COMP-5.
           03  W75-DNO        PIC X(06) OCCURS 18.

       01  W95-STM.
           03  W95-COMP        PIC X(40).

       01  W100-HEAD.
          03  FILLER        PIC X(36)    VALUE
              "DEBTOR ACCOUNTS: CONTRACT LISTING -".
          03  W100-DESC       PIC X(28).

      *
      *    **        ********  **    **  **    **    ****     ******   ********
      *    **           **     ***   **  **   **    **  **   **    **  **
      *    **           **     ****  **  **  **    **    **  **        ** 
      *    **           **     ** ** **  *****     ********  **        *****
      *    **           **     **  ****  **  **    **    **  **   ***  **  
      *    **           **     **   ***  **   **   **    **  **    **  **
      *    ********  ********  **    **  **    **  **    **   ******   ********
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

       01  S01.
    03  LINE  2 COLUMN 25 FOREGROUND-COLOR Grey HIGHLIGHT VALUE
    "RENTAL/SERVICE CONTRACT REPORTS".
    03  LINE  4 COLUMN 10 FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "R".
    03        COLUMN 11 VALUE "ental, ".
    03        COLUMN 18 FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "S".
    03        COLUMN 19 VALUE "ervice or ".
    03        COLUMN 29 FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "A".
    03        COLUMN 30 VALUE "ll contracts".

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
       AA00-MAIN       SECTION.
       AA00-INIT.
           IF LS0-DBLEV < 3
      *
      *      Insufficient security level
      *
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
               GO TO AA49.
             PERFORM ZA00-INIT.
             PERFORM DA000.
             CLOSE DBFOTO.

       AA49.
            EXIT PROGRAM.

       COPY "AA900.ALM".
       
       COPY "FUNCTION.SCR".

       COPY "ERROR.SCR".

       COPY "LOCKED.REC".

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³           ERASE SCREEN FROM SPECIFIED LOCATION            ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ Uses CRT-START as starting line and increases and CRT-END ³
      *    ³ as the ending line. The screen is cleared with Column 1   ³
      *    ³ and 80 containing "³" and columns 2 to 79 containing      ³
      *    ³ spaces.                                                   ³
      *    ³                                                           ³
      *    ³  eg.                                                      ³
      *    ³     MOVE 8              TO CRT-START.                     ³
      *    ³     MOVE 28             TO CRT-END.                       ³
      *    ³     PERFORM ERASE-SCREEN.                                 ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³             CLEAR MESSAGE LINE (Line 50)                  ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³                                                           ³
      *    ³     PERFORM CLEAR-L50                                     ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³             CHANGE THE TIME ON THE SCREEN                 ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³                                                           ³
      *    ³     PERFORM CHANGE-TIME.                                  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "CLEAR.CRT".

       COPY "ABORT.PRN".

       COPY "PRINTUTL.AP5".

      /
      *    ****    READ FILES ROUTINES
      *
       AC000-READ              SECTION.

       COPY "CONTROL.RD".

       COPY "PARAM.RD".

       COPY "DEBTOR.RD1".

       COPY "DBFOTO.RD".

       COPY "APAC.HLP".

       CA165-NAME        SECTION 2.
       CA165-EXTRACT.
            MOVE ZERO   TO WS-ERROR.
             UNSTRING W60-NAME DELIMITED BY "*" OR "!"
           INTO W40-SURNAME
          W40-FNAME
          W40-SNAME
               OVERFLOW
         MOVE 1   TO WS-ERROR.

       CA170-EXIT.
             EXIT.

      /
      *    ****    D E B T O R  R E N T A L   M A I N T E N A N C E
      *
       DA000        SECTION 5.
       DA00.
             PERFORM ERASE-SCREEN.
            DISPLAY S01.
      MOVE "A"   TO WS-OPTION.

       DA05.
      ACCEPT WS-OPTION AT 0443
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta UPDATE AUTO.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO DA999
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO DA05
        END-EVALUATE.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
    IF WS-OPTION = SPACES
        GO TO DA999.
          IF NOT (WS-OPTION = "A" OR "R" OR "S")
        GO TO DA05.
      MOVE WS-OPTION  TO WS-SEQ.
    IF WS-SEQ = "A"
              DISPLAY "Busy printing Rental & Service contracts"
   AT 0810
        MOVE "RENTAL AND SERVICE AGREEMENTS"
     TO W100-DESC
    ELSE
    IF WS-SEQ = "R"
              DISPLAY "Busy printing Rental contracts" AT 0810
        MOVE "RENTAL AGREEMENTS"
     TO W100-DESC
    ELSE
              DISPLAY "Busy printing Service contracts" AT 0810
        MOVE "SERVICE AGREEMENTS"
     TO W100-DESC.
      DISPLAY "Reading :" AT 1010.
      PERFORM SAVE-SCREEN-3.
      *
      *  ***  ****  ***** *   *   ****  ****  *** *   * ***** ***** ****
      * *   * *   * *   **  *   *   * *   *  *  **  *   *   *     * *
      * *   * ****  ***   * * *   ****  ****   *  * * *   *   ***   ****
      * *   * *     *   *  **   *     *   *  *  *  **   *   *     * *
      *  ***  *     ***** *   *   *     *   * *** *   *   *   ***** * *
      *
      PERFORM OPEN-PRINTER.
      MOVE "P"   TO WS-COMMAND.
      MOVE 0   TO WS-ADVANCE.
      MOVE 3   TO W02-PRN-TYPE.
      PERFORM CALL-PRINTUTL.
      PERFORM RESTORE-SCREEN-3.

       DA10.
      MOVE "H"   TO W02-PRN-TYPE.
            MOVE SPACES  TO R-DET.
            MOVE "PAGE:"  TO REP-PAGE.
            MOVE "DATE:"  TO REP-DATE.
            ADD 1   TO WS-PAGE.
            MOVE W12-TODAY  TO REP-DTE.
            MOVE WS-PAGE  TO REP-P-NO.
            MOVE W100-HEAD  TO REP-RESTART.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      MOVE SPACES  TO R-DET.
      *
      *    ****   T E S T   I F   O U T P U T T O   P R I N T E R
      *    O R S P O O L  F I L E
      *
    IF NOT (WS-PRINTER = "S")
        MOVE "E"   TO W02-PRN-TYPE
        PERFORM CALL-PRINTUTL
        MOVE SPACES  TO R-L1.
      MOVE "H"   TO W02-PRN-TYPE.
            MOVE W95-COMP  TO REP-CO.
      *
      *    ****   T E S T   I F   O U T P U T T O   P R I N T E R
      *    O R S P O O L  F I L E
      *
    IF NOT (WS-PRINTER = "S")
        MOVE 0   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      MOVE SPACES  TO R-L1.
    IF NOT (WS-PRINTER = "S")
        MOVE 0   TO WS-ADVANCE
        MOVE "X"   TO W02-PRN-TYPE
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.
            MOVE "AC-NO"  TO REP-HEAD7.
      MOVE "ACCOUNT NAME" TO REP-HEAD8.
      MOVE "CONT-NO"  TO REP-HEAD9.
      MOVE "CONTRACT DESCRIPTION"
     TO REP-HEAD10.
      MOVE "   PAYMENT"  TO REP-HEAD11.
      MOVE "TYPE"  TO REP-HEAD12.
      MOVE "FREQ"  TO REP-HEAD13.
      MOVE "ESCAL"  TO REP-HEAD14.
      MOVE "ANV-DATE"  TO REP-HEAD15.
      MOVE " UNIT RATE"  TO REP-HEAD16.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES TO R-DET.
      MOVE "D"   TO W02-PRN-TYPE.

       DA15.
      MOVE " !"   TO DBF-AC.
      PERFORM START-AT-DBFT-KEY THRU READ-DBFOTO-EXIT.
    IF WS-F-ERROR = 42
        GO TO DA25.

       DA20.
      PERFORM PRINT-ABORT.
    IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
              MOVE "**** PRINT ABORTED BY OPERATOR ****"
     TO R-DET
        GO TO DA30.
      PERFORM READ-DBFOTO-NEXT THRU READ-DBFOTO-EXIT.
    IF WS-F-ERROR = 42
        GO TO DA25.
      *
      *    ****   T E S T   I F   O U T P U T T O   P R I N T E R
      *    O R S P O O L  F I L E
      *
    IF NOT (WS-PRINTER = "S")
        DISPLAY DBF-AC AT 1020
         WITH FOREGROUND-COLOR Brown HIGHLIGHT
       BACKGROUND-COLOR Magenta
        DISPLAY DBF-CONT AT 1027
         WITH FOREGROUND-COLOR Cyan HIGHLIGHT
       BACKGROUND-COLOR Magenta.
    IF WS-SEQ = "R" OR "S"
        IF NOT (WS-SEQ = DBF-TYPE)
     GO TO DA20.
    IF DBF-AC NOT = DEB-ACNO
        MOVE DBF-AC  TO DEB-ACNO
        PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT
        MOVE DEB-ACNO  TO REP-AC
              INSPECT DEB-NAME REPLACING FIRST "*" BY ","
               INSPECT DEB-NAME REPLACING FIRST "*" BY " "
              INSPECT DEB-NAME REPLACING FIRST "!" BY " "
        MOVE DEB-NAME  TO REP-NAME.
      MOVE DBF-CONT  TO REP-CONT.
      MOVE DBF-DESC  TO REP-DESC.
      MOVE DBF-RENTAL  TO REP-INST.
    IF DBF-TYPE = "R"
        MOVE "RENT"  TO REP-TYPE
    ELSE
    IF DBF-TYPE = "S"
        MOVE "SERV"  TO REP-TYPE
    ELSE
        MOVE "OTHR"  TO REP-TYPE.
    IF DBF-FREQ = "1"
        MOVE "Mthly"  TO REP-FREQ
    ELSE
    IF DBF-FREQ = "2"
        MOVE "2 Mnth"  TO REP-FREQ
    ELSE
    IF DBF-FREQ = "3"
        MOVE "3 Mnth"  TO REP-FREQ
    ELSE
    IF DBF-FREQ = "4"
        MOVE "4 Mnth"  TO REP-FREQ
    ELSE
    IF DBF-FREQ = "6"
        MOVE "6 Mnth"  TO REP-FREQ
    ELSE
    IF DBF-FREQ = "A"
        MOVE "Annual"  TO REP-FREQ.
      MOVE DBF-ESC  TO REP-ESC.
      MOVE DBF-ADATE  TO REP-ADATE.
      MOVE DBF-RATE  TO REP-RATE.
    IF W02-LINAGE < (W02-PRN-LENGTH - 2)
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              PERFORM DA10.
            MOVE SPACES  TO R-DET.
      GO TO DA20.

       DA25.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      MOVE "****    E N D   O F   R E P O R T   ****"
     TO REP-CO.

       DA30.
      MOVE 99   TO WS-ADVANCE.
      MOVE "E"   TO WS-COMMAND.
      PERFORM CALL-PRINTUTL.
      MOVE "P"   TO WS-COMMAND.
      MOVE "C"   TO WS-COMMAND.
      PERFORM CALL-PRINTUTL.

       DA999.
      EXIT.

      /
       ZA00-INIT       SECTION 9.
       ZA00-OPEN.
             MOVE LS-PARID           TO WS-PARID.
             MOVE LS-TODAY-DDMMYY    TO TODAY-DDMMYY.
             MOVE LS-USUB            TO WS-USUB.
             MOVE LS-COMPANY         TO CRT-COMPANY.
             MOVE LS-TERMINAL        TO CRT-TERMINAL.
             PERFORM HEADING-AND-CLEAR-SCREEN.      
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

             COPY "FUNCTION.PRO".

            OPEN INPUT DBFOTO.
          IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
               IF IDX-CORRUPT
     MOVE 42  TO WS-F-ERROR
                   GO TO ZA50.
       ZA02.
            MOVE 11   TO WS-PARKEY.
            MOVE 1   TO WS-SUB-2.

       ZA15-READ-TRANS.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.

       ZA25.
             MOVE 1                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-COMPANY        TO W95-COMP.
             MOVE PAR-DMY            TO W12-TODAY.
             MOVE PAR-YMD            TO W12-T-YMD.
             MOVE 1                  TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
             GO TO ZA999-EXIT.

       COPY "ZA49.PRO".

       ZA50.
           IF WS-F-ERROR = 42
               MOVE "RENTAL file"    TO WS-FILE.
             DISPLAY "Rebuild "                  AT 0812 WS-FILE WITH FOREGROUND-COLOR Brown HIGHLIGHT .
             DISPLAY "Press any key to continue" AT 1012.
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       ZA51.
             EXIT PROGRAM.

       ZA200.

       COPY "LOCKED.PRO".

       ZA205.
             EXIT PROGRAM.

       ZA999-EXIT.
             EXIT.
      /
       ZB000       SECTION 9.

       COPY "ERRORS.PRO".

       DISPLAY-FILE-NAME.
             MOVE ZERO               TO WS-KEY.
             EVALUATE WS-F-ERROR
               WHEN 2          MOVE W02-NETWORK   TO WS-FILE
                               MOVE WS-NETKEY     TO WS-KEY
               WHEN 6          MOVE W02-DEBTOR    TO WS-FILE
                               MOVE DEB-ACNO      TO WS-KEYX
               WHEN 15         MOVE WS-PARID      TO WS-FILE
                               MOVE WS-PARKEY     TO WS-KEY
               WHEN 42         MOVE W02-DBFOTO    TO WS-FILE
                               MOVE DBF-KEY       TO WS-KEYX
               WHEN OTHER      MOVE "ERROR"       TO WS-FILE
                               MOVE "DTP032"      TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
