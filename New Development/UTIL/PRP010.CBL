      $SET LINKCOUNT"312" GNT"PRP010.GNT"
      ******************************************************************
      *                                                                *
      *   *******   *******   *******     ****     **       ****       *
      *   **    **  **    **  **    **   **  **    ***     **  **      *
      *   **    **  **    **  **    **  **    **    **    **    **     *
      *   *******   ******    *******   **    **    **    **    **     *
      *   **        **   **   **        **    **    **    **    **     *
      *   **        **    **  **         **  **     **     **  **      *
      *   **        **    **  **          ****    ******    ****       *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      * P A R A M E T E R   C R E D I T O R   T R A N S A C T I O N S  *
      *                                                                *
      *       Version 9.04.01 - May 2016                               *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     PRP010.
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

       COPY "GACCNT.SL".

       COPY "PARAM.SL".

       COPY "SYSTEM.SL".

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

       COPY "GACCNT.FDE".

       COPY "PARAM.FDE".

       COPY "SYSTEM.FD".

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
       77  WS-SECKEY               PIC  9(04)    COMP-5.
       77  WS-NUM                  PIC  Z9.
       77  WS-SNO                  PIC  9(02).
       77  WS-ADV-RATE             PIC S9(03)V99 COMP-3.
       77  WS-PAGE                 PIC  9(04)    COMP-5.
       77  WS-PGE-LENGTH           PIC  9(02)    VALUE 66.
       77  WS-PRN-LENGTH           PIC  9(02)    VALUE 62.
       77  WS-OPEN                 PIC  9(02)    VALUE ZERO.
       77  WS-OPTION               PIC  X(01).
       77  WS-INSTR                PIC  X(01).
       77  WS-VAT-GST              PIC  X(01).
       77  WS-ERROR                PIC  9(01).
       77  WS-PRN-NO               PIC  X(01)    COMP-X VALUE 9.
       77  WS-PRN-STAT             PIC  X(01)    COMP-X.
       77  WS-CHAR1                PIC  X(01).
       77  WS-CHAR2                PIC  X(01).
       77  WS-COMP                 PIC  9(01).
       77  WS-TYPE                 PIC  X(01).
       77  WS-ER6                  PIC  X(09)    VALUE "No Record".
       77  WS-ER7                  PIC  X(13)    VALUE "Group Heading".
       77  TODAY-DDMMYY            PIC  9(08)    COMP-5.
       77  WS-USUB                 PIC  9(04)    COMP-5.
      *    здддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё Used by calling program (WORKING-STORAGE) and called program Ё
      *    Ё (LINKAGE SECTION) for Main screen layout and headings.       Ё
      *    юдддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "PRP010".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "UTILITIES - CREDITOR TRANSACTION CODES".
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

       01  WS-HELP.
           03  WS-MODULE           PIC  X(03) VALUE "PRP".
           03  WS-PROG             PIC  X(03) VALUE "010".

       01  WS-PARID.
           03  WS-SYS-ID           PIC  X(03).

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

       01  W15-INPUT.
           03  W15-ACNT            PIC  9(06).
           03  W15-ACCOUNT         PIC  9(06).
           03  W15-ACNO1                         REDEFINES W15-ACCOUNT.
               05  W15-ACNO        PIC  9(04).
               05  W15-SER         PIC  9(02).
           03  W15-ACNO2                         REDEFINES W15-ACCOUNT.
               05  W15-GR          PIC  9(02).
               05  W15-SGRP        PIC  9(02).
               05  W15-ASER        PIC  9(02).
           03  W15-ACNO3                         REDEFINES W15-ACCOUNT.
               05  W15-GROUP       PIC  9(02).
               05  W15-REST        PIC  9(04).
           03  W15-ACNO4                         REDEFINES W15-ACCOUNT.
               05  W15-GRP         PIC  9(01).
               05  FILLER          PIC  9(05).

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

       01  W46-TRAN.
           03  W46-CODE            PIC  9(02).
           03  W46-DESC            PIC  X(15).
           03  W46-ABRV            PIC  X(03).
           03  W46-ACT             PIC  X(01).
           03  W46-T-GL            PIC  9(06).

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
                   07  W50-CHAR11  PIC  X(01)    OCCURS 4.
           03  W50-PRINTER         PIC  9(01).
           03  W50-TERMINAL        PIC  9(03).

       COPY "FUNCTION.WS".

       01  W75-KEYS.
           03  W75-S               PIC  9(02)    COMP-5.
           03  W75-S1              PIC  9(02)    COMP-5.
           03  W75-KEY             PIC  9(06)    COMP-5.
           03  W75-GNO             PIC  9(06)    COMP-5 OCCURS 18.

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
           03  LINE  2 COLUMN 27 FOREGROUND-COLOR Grey HIGHLIGHT VALUE "CREDITOR TRANSACTION OPTIONS".
           03  LINE  4 COLUMN 12 FOREGROUND-COLOR Brown HIGHLIGHT VALUE "M".
           03          COLUMN 13                              VALUE "aintain, ".
           03          COLUMN 22 FOREGROUND-COLOR Brown HIGHLIGHT VALUE "P".
           03          COLUMN 23                              VALUE "rint or ".
           03          COLUMN 31 FOREGROUND-COLOR Brown HIGHLIGHT VALUE "E".
           03          COLUMN 32                              VALUE "xit ? > <".

       01  S44.
           03  LINE  2 COLUMN 29 FOREGROUND-COLOR Grey HIGHLIGHT VALUE "CREDITOR TRANSACTION CODES".
           03  LINE  4 COLUMN 20                     VALUE "Code (01 to 20)".
           03  LINE  5 COLUMN 20                     VALUE "Description    ".
           03  LINE  6 COLUMN 20                     VALUE "Abreviation    ".
           03  LINE  7 COLUMN 20                     VALUE "Debit / Credit ".
      03  LINE  8 COLUMN 20                     VALUE "G/L Account No.".

       01  S49.
           03  S49A.
               05  LINE 4 COLUMN 36 FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Red PIC Z9 USING W46-CODE AUTO.
      03  S49AB.
               04  S49B.
                   05  LINE 5 COLUMN 36 FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Red PIC  X(15) USING W46-DESC AUTO.
              05  LINE 6 COLUMN 36 FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Red PIC  X(03) USING W46-ABRV AUTO.
              05  LINE 7 COLUMN 36 FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Red PIC  X(01) USING W46-ACT AUTO.
               04  S49C.
                   05  LINE 8 COLUMN 36 FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Red PIC ZZZZZ9 USING W46-T-GL AUTO.


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
           IF LS0-LEV > 0
               CLOSE GACCNT.

       AA49.
             EXIT PROGRAM.

       COPY "AA900.ALM".

       COPY "LOCKED.RC2".

      *    
      *            ROUTINES TO HANDLE VARIOUS FUNCTIONS FOR THE
      *           S C R E E N ,   K E Y B O A R D   &   M O U S E
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё      SAVE-SCREEN /-2/-3  and  RESTORE-SCREEN /-2/-3       Ё
      *    фммммммммммммммммммммммммммммммммммммммммммммммммммммммммммм╣
      *    Ё                      SCREEN-SHADOW                        Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё To routine is used to display a shadow down the right and Ё
      *    Ё along the bottom of a pop-up box. The parameters that are Ё
      *    Ё required:                                                 Ё
      *    Ё          SHADE-ROW   - Top line of the box.               Ё
      *    Ё          SHADE-COL   - Left line of box.                  Ё
      *    Ё          SHADE-WIDTH - Width of the box.                  Ё
      *    Ё          SHADE-LINES - Height of box.                     Ё
      *    фммммммммммммммммммммммммммммммммммммммммммммммммммммммммммм╣
      *    Ё                      CHECK-CORRECT                        Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё This routine displays a pop-up window with the message    Ё
      *    Ё           "Press Y if correct - N if incorrect"           Ё
      *    Ё                                                           Ё
      *    Ё The response is returned in WS-OPTION (in upper case).    Ё
      *    Ё                                                           Ё
      *    Ё To start with the pop-up window higher or lower than row  Ё
      *    Ё 20 (default); move another value to WS-MES-LINE.          Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
      *            T H I S   R O U T I N E   I S   U S E D   T O
      *       D I S P L A Y   I N F O R M A T I O N   M E S S A G E S
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё  Move the message to WS-DSP-MES and the top line of the   Ё
      *    Ё  message box to WS-MES-LINE. If WS-MES-LINE is zero the   Ё
      *    Ё  default will be 20.                                      Ё
      *    Ё                  PERFORM DISPLAY-MESSAGE                  Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
      *           T H I S   R O U T I N E   I S   U S E D   T O
      *            D I S P L A Y   E R R O R   M E S S A G E S
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                      ERROR-MESSAGE                        Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё To display the error message higher or lower (default is  Ё
      *    Ё line 20) Move the line number which must be used as the   Ё
      *    Ё top line to WS-MES-LINE. The message to be displayed must Ё
      *    Ё be in WS-ERR-MES. PERFORM ERROR-MESSAGE.                  Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       COPY "FUNCTION.CRT".

       COPY "ABORT.PRN".

       COPY "PRINTUTL.AP3".

       COPY "APAC.HLP".

       AA350-LOOKUP    SECTION.
       AA350.
             PERFORM SAVE-SCREEN.
      *      MOVE 4                  TO W44-FUNCTION.
             PERFORM SCREEN-CONTENTS.
             PERFORM GACCNT-LOOKUP.
             PERFORM RESTORE-SCREEN.

       AA399.
             EXIT.

      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                                                           Ё
      *    Ё           R E A D   F I L E S   R O U T I N E S           Ё
      *    Ё                                                           Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       AC000-READ              SECTION.

       COPY "CONTROL.RD".

       COPY "GACCNT.RD".

       COPY "PARAM.RD2".

      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                                                           Ё
      *    ЁR E W R I T E   &   W R I T E   F I L E S   R O U T I N E SЁ
      *    Ё                                                           Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       AD000-WRITE             SECTION.

       COPY "CONTROL.WR".

       COPY "PARAM.WR".

       COPY "GACCNT.LUP".

       BA000        SECTION.
       BA00.
             PERFORM ERASE-SCREEN.
             DISPLAY S10.
             MOVE "M"              TO WS-OPTION.
       BA05.
             ACCEPT WS-OPTION AT 0439 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta UPDATE AUTO.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
                      GO TO BA999
                 WHEN OTHER
                      PERFORM AA900-ALARM
                      GO TO BA05
               END-EVALUATE.
             CALL "CBL_TOUPPER" USING WS-OPTION BY VALUE WS-LENGTH RETURNING WS-STATUS.
           IF NOT(WS-OPTION = "E" OR "M" OR "P")
               GO TO BA05.
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

       YA000        SECTION 5.
       YA425-TRAN-CODES.
             PERFORM ERASE-SCREEN.
             DISPLAY S44.
       YA430-ACCEPT.
             MOVE ZERO             TO W46-CODE.
             ACCEPT S49A.
             DISPLAY SPACE AT 2501 WITH FOREGROUND-COLOR Cyan.
           IF W46-CODE = ZERO
               GO TO YA999.
           IF W46-CODE < 01 OR > 20
               MOVE "Invalid code" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO YA430-ACCEPT.
             ADD 381 W46-CODE      GIVING W90-RESULT.
             DIVIDE W90-RESULT BY 2 GIVING W90-TRNKEY REMAINDER W90-TS.
             ADD 1                 TO W90-TS.
             MOVE W90-TRNKEY       TO WS-PARKEY
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-C-DESC(W90-TS) TO W46-DESC.
             MOVE PAR-C-ABR (W90-TS) TO W46-ABRV.
           IF PAR-C-ACT(W90-TS) = 1
               MOVE "D"            TO W46-ACT
           ELSE
               MOVE "C"            TO W46-ACT.
             MOVE PAR-C-GL(W90-TS) TO W46-T-GL.
             DISPLAY S49AB.

       YA435-DESC.
             ACCEPT S49B.
             CALL "CBL_TOUPPER" USING W46-ACT BY VALUE WS-LENGTH RETURNING WS-STATUS.
           IF W46-ACT < "C" OR > "D"
               MOVE "Invalid Action code"  TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO YA435-DESC.
             DISPLAY "F1" AT 2502 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT "=Help," WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
           "F2" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT " to do Account Lookup - " WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
           "Esc" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT "ape to exit" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.

       YA440.
             ACCEPT S49C.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
                      GO TO YA999
                 WHEN F1-KEY
                      PERFORM HELP-ROUTINE
                 WHEN F2-KEY
                      MOVE W46-T-GL  TO W15-ACCOUNT
                      PERFORM AA350
                 WHEN OTHER
                      PERFORM AA900-ALARM
                      GO TO YA440
               END-EVALUATE
               MOVE W15-ACCOUNT    TO W46-T-GL
               DISPLAY S49C
               IF W46-T-GL = ZERO
                   GO TO YA440
               END-IF
           ELSE
           IF ADIS-FUNC
               EVALUATE KEY-CODE-1
                 WHEN UP-KEY
                      GO TO YA435-DESC
               END-EVALUATE.
           IF W46-T-GL = ZERO
               GO TO YA440.
             MOVE W46-T-GL         TO W15-ACCOUNT G-AC.
           IF W15-ASER = ZERO
               MOVE WS-ER7         TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO YA440.
             PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
           IF WS-F-ERROR = 8
               MOVE WS-ER6         TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO YA440.
             PERFORM CLEAR-L50.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               GO TO YA435-DESC.
             PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
             MOVE W46-DESC         TO PAR-C-DESC(W90-TS).
             MOVE W46-ABRV         TO PAR-C-ABR(W90-TS).
             MOVE W46-CODE         TO PAR-C-CODE(W90-TS).
           IF W46-ACT = "C"
               MOVE 0              TO PAR-C-ACT(W90-TS)
           ELSE
               MOVE 1              TO PAR-C-ACT(W90-TS).
             MOVE W46-T-GL         TO PAR-C-GL(W90-TS).
             PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
             GO TO YA425-TRAN-CODES.

       YA999.
             EXIT.

       YB000        SECTION 5.
       YB00.
             DISPLAY "Busy Printing" AT 1212 WITH FOREGROUND-COLOR Brown HIGHLIGHT BLINK.
      *
      *  ***  ****  ***** *   *   ****  ****  *** *   * ***** ***** ****
      * *   * *   * *     **  *   *   * *   *  *  **  *   *   *     *   *
      * *   * ****  ***   * * *   ****  ****   *  * * *   *   ***   ****
      * *   * *     *     *  **   *     *   *  *  *  **   *   *     *   *
      *  ***  *     ***** *   *   *     *   * *** *   *   *   ***** *   *
      *
             PERFORM OPEN-PRINTER.
             MOVE "P"              TO WS-COMMAND.

       YB100-TRANS.
             MOVE "H"              TO W02-PRN-TYPE.
             MOVE SPACES           TO REP-DETAIL1.
             MOVE "PAGE:"          TO REP-PAGE.
             ADD  1                TO WS-PAGE.
             MOVE WS-PAGE          TO REP-P-NO.
             MOVE "DATE:"          TO REP-DATE.
             MOVE W12-TODAY        TO REP-DTE.
             MOVE "CREDITOR TRANSACTION CODES" TO REP-COMPANY.
             MOVE 2                TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES           TO REP-DETAIL2.
             MOVE "CODE"           TO REP-HEAD1.
             MOVE "DESCRIPTION"    TO REP-HEAD2.
             MOVE "ABREVIATION"    TO REP-HEAD3.
             MOVE "ACTION"         TO REP-HEAD4.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES           TO REP-DETAIL2.
             MOVE "D"              TO W02-PRN-TYPE.

       YB105.
             MOVE 191              TO WS-PARKEY.

       YB110-LOOP.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE 1                TO W90-TS.

       YB112.
           IF PAR-C-CODE (W90-TS) = ZERO
               GO TO YB120-CHECK-END.
             MOVE PAR-C-CODE(W90-TS) TO REP-TRAN.
             MOVE PAR-C-DESC(W90-TS) TO REP-ENG.
             MOVE PAR-C-ABR(W90-TS)  TO REP-AFR.
           IF PAR-ACTION (W90-TS) = 1
               MOVE "C"            TO REP-ACT
           ELSE
               MOVE "D"            TO REP-ACT.
           IF W02-LINAGE < (WS-PRN-LENGTH - 2)
               MOVE 2              TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
           ELSE
               MOVE 99             TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               PERFORM YB100-TRANS.
             MOVE SPACES           TO REP-DETAIL2.
             PERFORM PRINT-ABORT.
           IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
               MOVE "**** PRINT ABORTED BY OPERATOR ****" TO REP-DETAIL2
               MOVE 99             TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               GO TO YB999.

       YB120-CHECK-END.
           IF W90-TS < 2
               ADD 1               TO W90-TS
               GO TO YB112.
           IF WS-PARKEY < 200
               ADD 1               TO WS-PARKEY
               GO TO YB110-LOOP.

       YB125.
             MOVE "**** END REPORT ****" TO REP-COMPANY.
             MOVE 99               TO WS-ADVANCE.
             MOVE "E"              TO WS-COMMAND.
             PERFORM CALL-PRINTUTL.
             MOVE "C"              TO WS-COMMAND.
             PERFORM CALL-PRINTUTL.

       YB999.
             EXIT.
      *    *************************************************************
      *                 I N I T I A L I S E   P R O G R A M
      *    *************************************************************
       ZA000-INIT        SECTION 8.
       ZA000-OPEN.
             MOVE LS-COMPANY         TO CRT-COMPANY.
             MOVE LS-TERMINAL        TO CRT-TERMINAL.
             MOVE LS-PARID           TO WS-PARID.
             MOVE LS-TODAY-DDMMYY    TO TODAY-DDMMYY W12-TODAY.
             MOVE LS-USUB            TO WS-USUB.
             PERFORM HEADING-AND-CLEAR-SCREEN.      
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё  Setup various keys and the mouse for use with menus and  Ё
      *    Ё  other required user interface functions and procedures.  Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

             COPY "FUNCTION.PRO".

       ZA00A-CONTINUE.
             MOVE 9                TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-VAT-GST      TO WS-VAT-GST.

       ZA27.
             MOVE W12-MONTH        TO WS-OPEN.
           IF LS0-LEV > 0
               OPEN INPUT GACCNT.
             OPEN INPUT SECUR.
             MOVE 1                TO WS-SECKEY.

       ZA28.
             READ SECUR.
           IF RUNTIME-ERROR
               IF RECORD-LOCKED
                   GO TO ZA28.
           IF WS-STATUS NOT = "00"
               DISPLAY "Disk error - ZA30" AT 2312 WITH FOREGROUND-COLOR Brown HIGHLIGHT
               STOP RUN.
             MOVE 1                TO WS-S1.

       ZA30.
             MOVE SEC-ASCII(WS-S1) TO W15-ASCII(WS-S1).
           IF WS-S1 < 256
               ADD 1               TO WS-S1
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
               WHEN 8          MOVE W02-GACCNT    TO WS-FILE
                               MOVE G-AC          TO WS-KEYX
               WHEN 15         MOVE WS-PARID      TO WS-FILE
                               MOVE WS-PARKEY     TO WS-KEY
               WHEN OTHER      MOVE "ERROR"       TO WS-FILE
                               MOVE "PRP010"      TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
