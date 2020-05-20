      $SET LINKCOUNT"320" GNT"PRP002.GNT"
      ******************************************************************
      *                                                                *
      *   *******   *******   *******     ****      ****     ******    *
      *   **    **  **    **  **    **   **  **    **  **   **    **   *
      *   **    **  **    **  **    **  **    **  **    **       **    *
      *   *******   ******    *******   **    **  **    **     **      *
      *   **        **   **   **        **    **  **    **   **        *
      *   **        **    **  **         **  **    **  **   **         *
      *   **        **    **  **          ****      ****    ********   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *    P A R A M E T E R   D E B T O R   C A T E G O R I E S       *
      *                                                                *
      *       Version 9.04.01 - May 2016                               *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     PRP002.
       AUTHOR.         J.W.LEMMON (APAC).
       DATE-WRITTEN.   JANUARY 1983.

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

       COPY "SYSTEM.SL".

       COPY "PARAM.SL".

       COPY "PRNTCONT.SL".

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

       COPY "SYSTEM.FD".

       COPY "PARAM.FDE".

       COPY "PRNTCONT.FD".

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
       77  WS-CHECK                PIC  X(18)    VALUE "aeWlimemnomLalismJ".
       77  WS-S1                   PIC  9(04)    COMP-5.
       77  WS-S2                   PIC  9(04)    COMP-5.
       77  WS-S3                   PIC  9(04)    COMP-5.
       77  WS-S4                   PIC  9(04)    COMP-5.
       77  WS-PARKEY               PIC  9(04)    COMP-5.
       77  WS-PRNKEY               PIC  9(04)    COMP-5.
       77  WS-DEPKEY               PIC  9(04)    COMP-5.
       77  WS-NETKEY               PIC  9(04)    COMP-5.
       77  WS-SECKEY               PIC  9(04)    COMP-5.
       77  WS-NUM                  PIC  Z9.
       77  WS-SNO                  PIC  9(02).
       77  WS-ADV-RATE             PIC S9(03)V99 COMP-3.
       77  WS-PAGE                 PIC  9(04)    COMP-5.
       77  PRG-PRINTUTL            PIC  X(12)    VALUE "UTP\PRINTUTL".
       77  WS-PGE-LENGTH           PIC  9(02)    VALUE 66.
       77  WS-PRN-LENGTH           PIC  9(02)    VALUE 62.
       77  WS-OPTION               PIC  X(01).
       77  WS-INSTR                PIC  X(01).
       77  WS-VAT-GST              PIC  X(01).
       77  WS-ERROR                PIC  9(01).
       77  WS-SELECT               PIC  X(13)    VALUE "Select Option".
       77  WS-PRN-NO               PIC  X(01)    COMP-X VALUE 9.
       77  WS-PRN-STAT             PIC  X(01)    COMP-X.
       77  WS-CHAR1                PIC  X(01).
       77  WS-CHAR2                PIC  X(01).
       77  WS-COMP                 PIC  9(01).
       77  WS-TYPE                 PIC  X(01).
       77  WS-INDEX-STORE          PIC  X(04).
       77  WS-INDEX-NAME           PIC  X(04).
       77  WS-BATCH                PIC  9(05).
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "PRP002".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "UTILITIES - DEBTOR CATERGORY CODES".
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

       01  W02-PRINTER-DETAILS.
           03  W02-PRINTER         PIC  X(12).
           03  W02-PGE-LENGTH      PIC  9(02).
           03  W02-PRN-LENGTH      PIC  9(02).
           03  W02-LINAGE          PIC  9(02).
           03  W02-PRN-STATUS      PIC  X(01)    VALUE "C".
      *
      *    ****    D  =  Detail line
      *            C  =  Condensed print
      *            E  =  Expanded print
      *            H  =  Header line
      *            1  =  10 Characters per inch
      *            2  =  12 Characters per inch
      *            3  =  17 Characters per inch
      *            6  =  6 Lines per inch
      *            8  =  8 Lines per inch
      *
           03  W02-PRN-TYPE        PIC  X(01).
           03  W02-PRN-LINE        PIC  X(136).
           03  REP-LINE1                         REDEFINES W02-PRN-LINE.
               05  REP-DETAIL1     PIC  X(136).
           03  REP-LINE2                         REDEFINES W02-PRN-LINE.
               05  REP-DETAIL2     PIC  X(80).
           03  REP-LINE3                         REDEFINES W02-PRN-LINE.
               05  REP-PAGE        PIC  X(06).
               05  REP-P-NO        PIC  Z(03)9.
               05  FILLER          PIC  X(09).
               05  REP-COMPANY     PIC  X(40).
               05  FILLER          PIC  X(05).
               05  REP-DATE        PIC  X(06).
               05  REP-DTE         PIC  Z9/99/9999.
           03  REP-LINE4                         REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(11).
               05  REP-HEAD1       PIC  X(04).
               05  FILLER          PIC  X(06).
               05  REP-HEAD2       PIC  X(15).
               05  FILLER          PIC  X(06).
               05  REP-HEAD3       PIC  X(15).
               05  FILLER          PIC  X(06).
               05  REP-HEAD4       PIC  X(06).
               05  FILLER          PIC  X(11).
           03  REP-LINE5                         REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(11).
               05  REP-TRAN        PIC  Z99.
               05  FILLER          PIC  X(09).
               05  REP-ENG         PIC  X(12).
               05  FILLER          PIC  X(09).
               05  REP-AFR         PIC  X(12).
               05  FILLER          PIC  X(10).
               05  REP-ACT         PIC  X(01).
               05  FILLER          PIC  X(13).

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

       01  W15-HEX.
           03  W15-ASCII           PIC  X(01) OCCURS 256.

       01  W25-CALCS.
           03  W25-RESULT          PIC  9(07)V99.
           03  W25-RESULT1                       REDEFINES W25-RESULT.
               05  FILLER          PIC  9(01).
               05  W25-WHOLE       PIC  9(06).
               05  W25-DECIMAL     PIC  9(02).
           03  W25-RESULT2                       REDEFINES W25-RESULT.
               05  W25-KEY         PIC  9(06).
               05  W25-SUB         PIC  9(01).
               05  FILLER          PIC  9(02).
           03  W25-PASS.
               05  W25-PASS1       PIC  9(03).
               05  W25-PASS2       PIC  9(03).
           03  W25-TIME            PIC  9(08).
           03  W25-TRED                          REDEFINES W25-TIME.
               05  FILLER          PIC  9(02).
               05  W25-FACT        PIC  9(06).
           03  W25-TOTAL           PIC S9(07)V99 COMP-3.
           03  W25-VALUE           PIC S9(07)V99 COMP-3.

       COPY "W40.WS".

       01  W50-PRINTERS.
           03  W50-TRM-PRINTER.
               05  W50-PICP        PIC  9(01).
               05  W50-PRN1        PIC  9(01).
               05  W50-INVP        PIC  9(01).
               05  W50-PRN2        PIC  9(01).
               05  W50-STDP        PIC  9(01).
               05  W50-PRN3        PIC  9(01).
               05  W50-STMP        PIC  9(01).
               05  W50-PRN4        PIC  9(01).
               05  W50-CSHP        PIC  9(01).
               05  W50-PRN5        PIC  9(01).
               05  W50-POS         PIC  9(01).
               05  W50-PRN6        PIC  9(01).
           03  W50-TRM-PRN-CONT.
      *            CONDENSED PRINT
               05  W50-CONP.
                   07  W50-CHAR1   PIC  X(01)    OCCURS 4.
      *            CANCEL CONDENSED PRINT
               05  W50-NORP.
                   07  W50-CHAR2   PIC  X(01)    OCCURS 4.
      *            EXPANDED PRINT
               05  W50-EXPP.
                   07  W50-CHAR3   PIC  X(01)    OCCURS 4.
      *            CANCEL EXPANDED PRINT
               05  W50-ECAN.
                   07  W50-CHAR4   PIC  X(01)    OCCURS 4.
      *            SET PAGE LENGTH TO 48
               05  W50-8LPI.
                   07  W50-CHAR5   PIC  X(01)    OCCURS 4.
      *            SET PAGE LENGTH TO 66
               05  W50-6LPI.
                   07  W50-CHAR6   PIC  X(01)    OCCURS 4.
      *            PRINT 10 CHARS PER INCH
               05  W50-10CPI.
                   07  W50-CHAR7   PIC  X(01)    OCCURS 4.
      *            PRINT 12 CHARS PER INCH
               05  W50-12CPI.
                   07  W50-CHAR8   PIC  X(01)    OCCURS 4.
      *            PRINT 17 CHARS PER INCH
               05  W50-17CPI.
                   07  W50-CHAR9   PIC  X(01)    OCCURS 4.
      *            SET DOUBLE PRINT
               05  W50-DBLP.
                   07  W50-CHAR10  PIC  X(01)    OCCURS 4.
      *            RELEASE DOUBLE PRINT
               05  W50-DCAN.
                   07  W50-CHAR11 PIC  X(01)     OCCURS 4.
           03  W50-PRINTER        PIC  9(01).
           03  W50-TERMINAL       PIC  9(03).

       COPY "FUNCTION.WS".

       01  W60-NME-ADD.
           03  W60-CSHEAD     PIC  X(32).
       01  W65-ENTRY.
           03  W65-FOUR.
               05  W65-CHAR OCCURS 4 PIC X(01).

       01  W75-TITLES.
           03  W75-DESCRIP.
               05  FILLER          PIC  X(28)    VALUE "Mr     Mnr    Mrs    Mev    ".
               05  FILLER          PIC  X(28)    VALUE "Miss   Mej    Ms     Ms     ".
               05  FILLER          PIC  X(28)    VALUE "Dr     Dr     Rev    Ds     ".
               05  FILLER          PIC  X(28)    VALUE "Prof   Prof   The HonSy Edel".
               05  FILLER          PIC  X(28)    VALUE "                            ".
           03  W75-CODES                         REDEFINES W75-DESCRIP.
               05  W75-TITLE                     OCCURS 10.
                   07  W75-ETITLE  PIC  X(07).
                   07  W75-ATITLE  PIC  X(07).

       01  W80-ACCOUNT.
           03  W80-ACNO            PIC  X(06).
           03  W80-NUMD                          REDEFINES W80-ACNO.
               05  W80-COMP        PIC  9(01).
               05  W80-TYPE        PIC  9(01).
               05  W80-SERIAL      PIC  9(04).
           03  W80-CASH-GL         PIC  9(06).
       01  W90-CALC-KEYS.
           03  W90-RESULT          PIC  9(04)    COMP-5.
           03  W90-TRNKEY          PIC  9(04)    COMP-5.
           03  W90-TS              PIC  9(02)    COMP-5.
           03  W90-CATKEY          PIC  9(04)    COMP-5.
           03  W90-CS              PIC  9(02)    COMP-5.

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

       01  S10.
           03  LINE  5 COLUMN 29 VALUE "DEBTOR CATEGORY OPTIONS" FOREGROUND-COLOR Grey HIGHLIGHT.
           03  BACKGROUND-COLOR Cyan FOREGROUND-COLOR Black.
               05  LINE  7 COLUMN 12 VALUE " aintain,  rint or  xit ? > <".
           03  BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 12 VALUE "M".
               05          COLUMN 22 VALUE           "P".
               05          COLUMN 31 VALUE                    "E".

       01  S15.
           03  LINE  5 COLUMN 33 VALUE "DEBTOR CATEGORIES" FOREGROUND-COLOR Grey HIGHLIGHT.
           03  BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
               05  LINE 7  COLUMN 19 VALUE "                                            " .
               05  LINE 8  COLUMN 19 VALUE " Code                                       " .
               05  LINE 9  COLUMN 19 VALUE " Description                                " .
               05  LINE 10 COLUMN 19 VALUE "                                            " .
           03  BACKGROUND-COLOR Red FOREGROUND-COLOR Black.
               05  LINE 8  COLUMN 32 VALUE              "  ".
               05  LINE 9  COLUMN 32 VALUE              "                              ".

       01  S30.
           03  BACKGROUND-COLOR Red FOREGROUND-COLOR Grey HIGHLIGHT.
               05  S30A.
                   07  PIC  Z9 USING WS-SNO AUTO.
               05  S30B.
                   07  PIC  X(30) USING PAR-CDESC(W90-CS) UPDATE AUTO.

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
       AA000-MAIN        SECTION.
       AA000-INIT.
           IF LS0-PALEV < 3
      *
      *      Insufficient security level
      *
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
               GO TO AA49.
             PERFORM ZA000-INIT.
             PERFORM BA000.

       AA49.
             EXIT PROGRAM.

       COPY "LOCKED.RC2".

      *    
      *            ROUTINES TO HANDLE VARIOUS FUNCTIONS FOR THE
      *           S C R E E N ,   K E Y B O A R D   &   M O U S E
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³      SAVE-SCREEN /-2/-3  and  RESTORE-SCREEN /-2/-3       ³
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ³                      SCREEN-SHADOW                        ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ To routine is used to display a shadow down the right and ³
      *    ³ along the bottom of a pop-up box. The parameters that are ³
      *    ³ required:                                                 ³
      *    ³          SHADE-ROW   - Top line of the box.               ³
      *    ³          SHADE-COL   - Left line of box.                  ³
      *    ³          SHADE-WIDTH - Width of the box.                  ³
      *    ³          SHADE-LINES - Height of box.                     ³
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ³                      CHECK-CORRECT                        ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ This routine displays a pop-up window with the message    ³
      *    ³           "Press Y if correct - N if incorrect"           ³
      *    ³                                                           ³
      *    ³ The response is returned in WS-OPTION (in upper case).    ³
      *    ³                                                           ³
      *    ³ To start with the pop-up window higher or lower than row  ³
      *    ³ 20 (default); move another value to WS-MES-LINE.          ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      *            T H I S   R O U T I N E   I S   U S E D   T O
      *       D I S P L A Y   I N F O R M A T I O N   M E S S A G E S
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Move the message to WS-DSP-MES and the top line of the   ³
      *    ³  message box to WS-MES-LINE. If WS-MES-LINE is zero the   ³
      *    ³  default will be 20.                                      ³
      *    ³                  PERFORM DISPLAY-MESSAGE                  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      *           T H I S   R O U T I N E   I S   U S E D   T O
      *            D I S P L A Y   E R R O R   M E S S A G E S
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                      ERROR-MESSAGE                        ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ To display the error message higher or lower (default is  ³
      *    ³ line 20) Move the line number which must be used as the   ³
      *    ³ top line to WS-MES-LINE. The message to be displayed must ³
      *    ³ be in WS-ERR-MES. PERFORM ERROR-MESSAGE.                  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "FUNCTION.CRT".

       COPY "ABORT.PRN".

       COPY "PRINTUTL.AP3".

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                                                           ³
      *    ³           R E A D   F I L E S   R O U T I N E S           ³
      *    ³                                                           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AC000-READ              SECTION.

       COPY "CONTROL.RD".

       COPY "PARAM.RD2".

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                                                           ³
      *    ³R E W R I T E   &   W R I T E   F I L E S   R O U T I N E S³
      *    ³                                                           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AD000-WRITE             SECTION.

       COPY "CONTROL.WR".

       COPY "PARAM.WR".

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                                                           ³
      *    ³      D E B T O R   C A T E R G O R Y   O P T I O N S      ³
      *    ³                                                           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       BA000        SECTION.
       BA00.
             PERFORM ERASE-SCREEN.
             DISPLAY S10.
             MOVE "M"                TO WS-OPTION.
             MOVE 7                  TO WS-MES-LINE.
             MOVE 39                 TO WS-MES-COL.
      *
      *      ACCEPT the option at the position specified above
      *
             MOVE X"FA"              TO WS-MES-CHAR(48).
             MOVE "d"                TO WS-INSTR.
           IF WS-OPTION = X"EC"
               GO TO BA999.
           IF WS-OPTION = "M"
               PERFORM YA000
           ELSE
           IF WS-OPTION = "P"
               PERFORM YB000
           ELSE
               GO TO BA999.
             GO TO BA00.

       BA999.
             EXIT.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                                                           ³
      *    ³  D E B T O R   C A T E R G O R Y   M A I N T E N A N C E  ³
      *    ³                                                           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       YA000        SECTION 5.
       YA195-CAT.
             PERFORM ERASE-SCREEN.
             DISPLAY S15.
             MOVE ZERO               TO WS-SNO.
       YA200.
             ACCEPT S30A AT 0832.
           IF WS-SNO = ZERO
              GO TO YA999.
           IF WS-SNO > 80
               GO TO YA200.
             ADD 121 WS-SNO          GIVING W90-RESULT.
             DIVIDE W90-RESULT BY 2  GIVING W90-CATKEY REMAINDER W90-CS.
             ADD 1                   TO W90-CS.
             MOVE W90-CATKEY         TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
       YA205.
             ACCEPT S30B AT 0932.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               GO TO YA205.
           IF PAR-CDESC(W90-CS) = SPACES
               MOVE ZERO             TO PAR-CAT(W90-CS) PAR-CDRMTD(W90-CS) PAR-CDRYTD(W90-CS) PAR-CCRMTD(W90-CS) PAR-CCRYTD(W90-CS)
           ELSE
               MOVE WS-SNO           TO PAR-CAT(W90-CS).
             PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
             GO TO YA195-CAT.
       YA999.
             EXIT.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                                                           ³
      *    ³      D E B T O R   C A T E R G O R Y   L I S T I N G      ³
      *    ³                                                           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       YB000        SECTION 5.
       YB00.
             DISPLAY "Busy Printing" AT 1212 WITH FOREGROUND-COLOR Brown HIGHLIGHT BLINK.
      *
      *  ***  ****  ***** *   *   ****  ****  *** *   * ***** ***** ****
      * *   * *   * *     **  *   *   * *   *  *  **  *   *   *     *   *
      * *   * ****  ***   * * *   ****  ****   *  * * *   *   ***   ****
      * *   * *     *     *  **   *     *   *  *  *  **   *   *     *  *
      *  ***  *     ***** *   *   *     *   * *** *   *   *   ***** *   *
      *
             PERFORM OPEN-PRINTER.
             MOVE "P"                TO WS-COMMAND.

       YB010-CAT.
             MOVE "H"                TO W02-PRN-TYPE.
             MOVE SPACES             TO REP-DETAIL1.
             MOVE "PAGE:"            TO REP-PAGE.
             ADD  1                  TO WS-PAGE.
             MOVE WS-PAGE            TO REP-P-NO.
             MOVE "DATE:"            TO REP-DATE.
             MOVE W12-TODAY          TO REP-DTE.
             MOVE "DEBTOR CATEGORY CODES" TO REP-COMPANY.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO REP-DETAIL2.
             MOVE "CODE"             TO REP-HEAD1.
             MOVE "DESCRIPTION"      TO REP-COMPANY.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO REP-DETAIL2.
             MOVE "D"                TO W02-PRN-TYPE.

       YB15.
             MOVE 61                 TO WS-PARKEY.

       YB020.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE 1                  TO W90-CS.

       YB22.
           IF PAR-CAT(W90-CS) = ZERO
               GO TO YB24.
             MOVE PAR-CAT(W90-CS)    TO REP-TRAN.
             MOVE PAR-CDESC(W90-CS)  TO REP-COMPANY.
           IF W02-LINAGE < (WS-PRN-LENGTH - 2)
               MOVE 2                TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
           ELSE
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               PERFORM YB010-CAT.
             PERFORM PRINT-ABORT.
           IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
               MOVE "**** PRINT ABORTED BY OPERATOR ****" TO REP-DETAIL2
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               GO TO YB999.

       YB24.
           IF W90-CS < 2
               ADD 1                 TO W90-CS
               GO TO YB22.
           IF WS-PARKEY < 100
               ADD 1                 TO WS-PARKEY
               GO TO YB020.
             MOVE "**** END REPORT ****" TO REP-DETAIL2.
             MOVE 99                 TO WS-ADVANCE.
             MOVE "E"                TO WS-COMMAND.
             PERFORM CALL-PRINTUTL.
             MOVE "C"                TO WS-COMMAND.
             PERFORM CALL-PRINTUTL.

       YB999.
             EXIT.
      *    *************************************************************
      *                I N I T I A L I S E   P R O G R A M
      *    *************************************************************
       ZA000-INIT        SECTION 8.
       ZA000-OPEN.
             MOVE LS-COMPANY         TO CRT-COMPANY.
             MOVE LS-TERMINAL        TO CRT-TERMINAL.
             MOVE LS-PARID           TO WS-PARID.
             MOVE LS-TODAY-DDMMYY    TO TODAY-DDMMYY W12-TODAY.
             MOVE LS-USUB            TO WS-USUB.
             PERFORM HEADING-AND-CLEAR-SCREEN.      
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

             COPY "FUNCTION.PRO".

       ZA00A-CONTINUE.
             MOVE 9                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-VAT-GST        TO WS-VAT-GST.
       ZA27.
             OPEN INPUT SECUR.
             MOVE 1                  TO WS-SECKEY.

       ZA28.
             READ SECUR.
           IF RUNTIME-ERROR
               IF RECORD-LOCKED
                   GO TO ZA28.
           IF WS-STATUS NOT = "00"
               DISPLAY "Disk error - ZA30" AT 2312
                        WITH FOREGROUND-COLOR Brown HIGHLIGHT
               STOP RUN.
             MOVE 1                  TO WS-S1.

       ZA30.
             MOVE SEC-ASCII(WS-S1)  TO W15-ASCII(WS-S1).
           IF WS-S1 < 256
               ADD 1                 TO WS-S1
               GO TO ZA30.
             CLOSE SECUR.
             GO TO ZA999-EXIT.

       ZA999-EXIT.
             EXIT.
      *
      *    ****    I / O   E R R O R   M E S S A G E S
      *
       ZB000-ERROR      SECTION 8.

       COPY "ERRORS.PRO".

       DISPLAY-FILE-NAME.
             MOVE ZERO               TO WS-KEY.
             EVALUATE WS-F-ERROR
               WHEN 15         MOVE WS-PARID      TO WS-FILE
                               MOVE WS-PARKEY     TO WS-KEY
               WHEN OTHER      MOVE "ERROR"       TO WS-FILE
                               MOVE "PRP002"      TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
