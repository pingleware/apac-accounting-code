      $SET LINKCOUNT"256"
      ******************************************************************
      *                                                                *
      *    *    ******     *     *****  ****** **   **  *****  ******  *
      *   ***   **   **   ***   **   **   **   ***  ** **   **   **    *
      *  ** **  **   **  ** **  **        **   **** ** **        **    *
      * **   ** ******  **   ** **        **   ** ****  *****    **    *
      * ******* **      ******* **        **   **  ***      **   **    *
      * **   ** **      **   ** **   **   **   **   ** **   **   **    *
      * **   ** **      **   **  *****  ****** **   **  *****    **    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     A P A C   I N I T I A L I Z E   P R O G R A M              *
      *                                                                *
      *       Version 9.04.02 - October 2016                           *
      *                                                                *
      ******************************************************************
      *                                                                *
      *    Version 8 has introduced editable file paths and allows for *
      *    the sharing of files between two businesses.                *
      *                                                                *
      *  Feb 2007 - New file (DEBDEP/AR_REST.DAT) - Debtor sales       *
      *             restrictions file has been included which          *
      *             will allow for departments to de excluded or       *
      *             only selected departments allowed. This will       *
      *             allow the user the abillity to allow or bar        *
      *             sales of selected departments.                     *
      *                                                                *
      *  Jan 2008 - New file (DEBALT) - Debtor alternate index         *
      *             included for lookups, using any word con-          *
      *             tained in the Debtor name.                         *
      *                                                                *
      *  March 2010 - Include facility to print to a USB printer       *
      *               via the Windows print using the relavent         *
      *               print API's supplied with MF COBOL. Allow        *
      *               BMP files in APACFILE (INVBMP & STMBMP).         *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     APACINST.
       AUTHOR.         J W LEMMON (APAC).
       DATE-WRITTEN.   APRIL 1991.

                COPYRIGHT NOTICE: COPYRIGHT (C) 1991 - 2017
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

       COPY "APACFIDS.SL".

       COPY "APCCOM.SL".

       COPY "AUDIT.SL".

       COPY "CONTROL.SL".

       COPY "CHEQUE.SL".

       COPY "DEPART.SL".

       COPY "PARAM.SL".

       COPY "SYSTEM.SL".

       COPY "SYSUSE.SL".

       COPY "TXTRAN.SL".

       DATA DIVISION.
       FILE SECTION.

       COPY "APACFIDS.FDE".

       COPY "APCCOM.FD".

       COPY "AUDIT.FD".

       COPY "CONTROL.FD".

       COPY "CHEQUE.FD".

       COPY "DEPART.FD".

       COPY "PARAM.FD".

       COPY "SYSTEM.FD".

       COPY "SYSUSE.FD".

       COPY "TXTRAN.FD".

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
       77  WS-AUDKEY               PIC  9(04)    COMP-5.
       77  WS-NETKEY               PIC  9(04)    COMP-5.
       77  WS-SECKEY               PIC  9(04)    COMP-5.
       77  WS-GST-RATE             PIC  9(03)V99 COMP-3.
       77  WS-LINES                PIC  9(02).
       77  WS-PAGE                 PIC  9(04)    COMP-5.
       77  WS-OPTION               PIC  X(01).
       77  WS-ERROR                PIC  9(01).
       77  WS-DEB                  PIC  9(05).
       77  WS-CON                  PIC  9(05).
       77  WS-NUM                  PIC  Z(04)9.
       77  WS-MES                  PIC  X(11)    VALUE "Initialized".
       77  TODAY-DDMMYY            PIC  9(08)    COMP-5.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Used by calling program (WORKING-STORAGE) and called program ³
      *    ³ (LINKAGE SECTION) for Main screen layout and headings.   ³
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "APACINST".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "INITIALIZE APAC SYSTEM FILES".
      *
      *    Company Name
      *
           03  CRT-COMPANY         PIC  X(40).
      *
      *    WorkStation / Supervisor
      *
           03  CRT-WRKHD           PIC  X(11).
           03  CRT-WRKST           PIC  X(03) VALUE "ÄÄÄ".

      /
       01  WS-DB-LINE.
           03  WS-TOP-LNE.
               05  WS-TCHR PIC  X(01) OCCURS 80.
           03  WS-T-LINE REDEFINES WS-TOP-LNE.
               05  FILLER  PIC  X(01).
               05  WS-H-LINE
                           PIC  X(78).
               05  FILLER  PIC  X(01).
           03  WS-TOP-LNE2.
               05  WS-TCH  PIC  X(01) OCCURS 80.
           03  WS-MID-LNE.
               05  WS-MCHR PIC  X(01) OCCURS 80.
           03  WS-MID-LNE2.
               05  FILLER      PIC  X(01) VALUE "³".
               05  WS-BLNK78   PIC  X(78) VALUE ALL "°".
               05  FILLER      PIC  X(01) VALUE "³".
          03  WS-BOT-LNE.
               05  WS-BCHR PIC  X(01) OCCURS 80.
           03  WS-B-LINE REDEFINES WS-BOT-LNE.
               05  FILLER  PIC  X(01).
               05  WS-F-LINE
                           PIC  X(78).
               05  FILLER  PIC  X(01).
           03  WS-BOT-LNE2.
               05  WS-BCH  PIC  X(01) OCCURS 80.

       COPY "WS.WS".

       01  W01-CONTROL.
           03  W01-COMP.
               05  W01-DEC     PIC  9(02) COMP-X.
           03  W01-DISP REDEFINES W01-COMP.
               05  W01-CHAR    PIC  X(01).
           03  W01-NUL         PIC  X(01).
           03  W01-ESC         PIC  X(01).
           03  W01-EXP         PIC  X(01).
           03  W01-ECAN        PIC  X(01).
           03  W01-NORM        PIC  X(01).
           03  W01-COND        PIC  X(01).
           03  W01-DBLE        PIC  X(01).
           03  W01-DCAN        PIC  X(01).
           03  W01-8LIN        PIC  X(01).
           03  W01-6LIN        PIC  X(01).

       01  WS-PARID.
          03  WS-SYS-ID       PIC  X(03).

       01  W02-FID.

       COPY "APACFIDS.ID".

       COPY "AUDIT.ID".

       COPY "CONTROL.ID".

       COPY "CHEQUE.ID".

       COPY "DEPART.ID".

       COPY "PARAM.ID".

       COPY "SYSTEM.ID".

       COPY "SYSUSE.ID".

       COPY "TXTRAN.ID".
      *
      *    CONTROL CHARACTERS FOR DOT MATRIX PRINTERS
      *
       01  W10-PRINTERS.
      *        CONDENSED PRINT
           03  W09-CONP.
               05  W10-CONP        PIC  X(04).
               05  FILLER          PIC  X(02)   VALUE X"0000".
      *        CANCEL CONDENSED PRINT
           03  W09-NORP.
               05  W10-NORP        PIC  X(04).
               05  FILLER          PIC  X(02)   VALUE X"0000".
      *        EXPANDED PRINT
           03  W09-EXPP.
               05  W10-EXPP        PIC  X(04).
               05  FILLER          PIC  X(02)   VALUE X"0000".
      *        CANCEL EXPANDED PRINT
           03  W09-ECAN.
               05  W10-ECAN        PIC  X(04).
               05  FILLER          PIC  X(02)   VALUE X"0000".
      *        SET SPACING TO 8 LPI
           03  W09-8LPI.
               05  W10-8LPI        PIC  X(04).
               05  FILLER          PIC  X(02)   VALUE X"0000".
      *        SET SPACING TO 6 LPI
           03  W09-6LPI.
               05  W10-6LPI        PIC  X(04).
               05  FILLER          PIC  X(02)   VALUE X"0000".
      *        PRINT 10 CHARS PER INCH
           03  W09-10CPI.
               05  W10-10CPI       PIC  X(04).
               05  FILLER          PIC  X(02)   VALUE X"0000".
      *        PRINT 12 CHARS PER INCH
           03  W09-12CPI.
               05  W10-12CPI       PIC  X(04).
               05  FILLER          PIC  X(02)   VALUE X"0000".
      *        PRINT 17 CHARS PER INCH
           03  W09-17CPI.
               05  W10-17CPI       PIC  X(04).
               05  FILLER          PIC  X(02)    VALUE X"0000".

       01  W12-TODAY.
           03  W12-TODAY-DDMMYY    PIC 9(08).
           03  W12-DATE                          REDEFINES W12-TODAY-DDMMYY.
               05  W12-DAY         PIC 9(02).
               05  W12-MONTH       PIC 9(02).
               05  W12-CENT        PIC 9(02).
               05  W12-YEAR        PIC 9(02).

       COPY "FUNCTION.WS".

       COPY "W40.WS".

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

       COPY "BLANK.CRT".

       01  S01.
           03  LINE  4 COLUMN  4 VALUE " Please be patient - Initializing files " FOREGROUND-COLOR Brown HIGHLIGHT.
           03  LINE  6 COLUMN  4 PIC X(18) USING WS-PARID.
           03  LINE  8 COLUMN  4 PIC X(20) USING W02-NETWORK.
           03  LINE 10 COLUMN  4 PIC X(19) USING W02-DEPART.
           03  LINE 12 COLUMN  4 PIC X(19) USING W02-AUDITF.
           03  LINE 14 COLUMN  4 PIC X(19) USING W02-TXTRAN.
           03  LINE 16 COLUMN  4 PIC X(19) USING W02-CHEQUE.
           03  LINE 18 COLUMN  4 PIC X(19) USING W02-SYSUSER.

       COPY "OPT.CRT".

       COPY "ERROR.CRT".

      *
      *      ******   ******    *****    *****   ******  ******   **   **  ******    ****** 
      *      **   **  **   **  **   **  **   **  **      **   **  **   **  **   **   **
      *      **   **  **  **   **   **  **       **      **   **  **   **  **  **    **
      *      ******   *****    **   **  **       *****   **   **  **   **  *****     *****
      *      **       **  **   **   **  **       **      **   **  **   **  **  **    **
      *      **       **   **  **   **  **   **  **      **   **  **   **  **   **   **
      *      **       **   **   *****    *****   ******  ******    *****   **   **   ******
      *
       PROCEDURE DIVISION USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
       AA000-MAIN              SECTION.
       AA00.
             MOVE 1   TO WS-S1.
             MOVE SPACES  TO WS-MID-LNE.
       AA02.
             MOVE WS-G1   TO WS-TCHR(WS-S1) WS-BCHR(WS-S1).
      MOVE WS-G8   TO WS-TCH(WS-S1)  WS-BCH(WS-S1).
           IF WS-S1 < 80
              ADD 1   TO WS-S1
               GO TO AA02.
      MOVE WS-G9   TO WS-TCH(1).
      MOVE WS-G10  TO WS-TCH(80).
      MOVE WS-G11  TO WS-BCH(1).
      MOVE WS-G12  TO WS-BCH(80).
      MOVE WS-G14  TO WS-TCHR(1) WS-BCHR(1).
      MOVE WS-G13  TO WS-TCHR(80) WS-BCHR(80).
            MOVE WS-G2   TO WS-TCHR(16) WS-TCHR(41)
              WS-TCHR(64) WS-TCHR(71).
            MOVE WS-G3   TO WS-MCHR(16) WS-MCHR(41)
              WS-MCHR(64) WS-MCHR(71)
        WS-MCHR(1) WS-MCHR(80) .
            MOVE WS-G4   TO WS-BCHR(16) WS-BCHR(41)
              WS-BCHR(64) WS-BCHR(71).
      MOVE LS-PARID  TO WS-PARID.
      MOVE LS-L-OR-N  TO W02-L-OR-N.
      MOVE WS-SYS-ID  TO W02-SYSID.
      MOVE LS-TODAY-DDMMYY
     TO TODAY-DDMMYY
        W12-TODAY-DDMMYY.
             PERFORM ERASE-SCREEN.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³     Setup the file ID's including path names           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      *    *************************************************************
      *    ****    P A R A M E T E R   F I L E      ****
      *    *************************************************************
      MOVE "PARAM"  TO AFID-KEY.

       AA03-READ-APACFIDS.
            READ APACFIDS WITH IGNORE LOCK
        KEY IS AFID-KEY.
    IF WS-STATUS = "00"
        GO TO AA03-READ-APACFIDS-EXIT.
            STRING "Missing " DELIMITED SIZE
       AFID-KEY DELIMITED BY " "
       " file ID - Status " DELIMITED SIZE
       WS-STATUS DELIMITED SIZE INTO WS-ERR-MES.
      PERFORM ERROR-LENGTH THRU ERROR-EXIT.
            STOP RUN.

       AA03-READ-APACFIDS-EXIT.
      EXIT.

       AA03A-CONTINUE.
      MOVE AFID-PATH  TO W02-PARAM.
      *    *************************************************************
      *    ****   A U D I T   F I L E      ****
      *    *************************************************************
      MOVE "AUDITF"  TO AFID-KEY.
      PERFORM AA03-READ-APACFIDS THRU AA03-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-AUDITF.
      *    *************************************************************
      *    ****         C H E Q U E   F I L E      ****
      *    *************************************************************
      MOVE "CHEQUE"  TO AFID-KEY.
      PERFORM AA03-READ-APACFIDS THRU AA03-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-CHEQUE.
      *    *************************************************************
      *    ****     D E P A R T M E N T  F I L E     ****
      *    *************************************************************
      MOVE "DEPART"  TO AFID-KEY.
      PERFORM AA03-READ-APACFIDS THRU AA03-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-DEPART.
      *    *************************************************************
      *    ****      C O N T R O L   F I L E      ****
      *    *************************************************************
      MOVE "NETWORK"  TO AFID-KEY.
      PERFORM AA03-READ-APACFIDS THRU AA03-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-NETWORK.
      *    *************************************************************
      *    ****         S Y S T E M   F I L E      ****
      *    *************************************************************
      MOVE "SYSTEM"  TO AFID-KEY.
      PERFORM AA03-READ-APACFIDS THRU AA03-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-SYSTEM.
      *    *************************************************************
      *    ****   U S E R S   F I L E      ****
      *    *************************************************************
      MOVE "SYSUSE"  TO AFID-KEY.
      PERFORM AA03-READ-APACFIDS THRU AA03-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-SYSUSER.
      *    *************************************************************
      *    ****         V . A . T .   F I L E      ****
      *    *************************************************************
      MOVE "TXTRAN"  TO AFID-KEY.
      PERFORM AA03-READ-APACFIDS THRU AA03-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-TXTRAN.
            OPEN I-O SECUR.
            MOVE 2   TO WS-SECKEY.
      OPEN I-O APCCOM.
    IF WS-STATUS NOT = "00"
        OPEN OUTPUT APCCOM
        INITIALIZE COM-RECORD
        MOVE "APC"  TO COM-CODE
        WRITE COM-RECORD
        CLOSE APCCOM
        OPEN I-O APCCOM.
             READ SECUR WAIT.
      MOVE WS-SYS-ID  TO COM-CODE.
      READ APCCOM.
      MOVE SEC-COMP  TO COM-NAME.
      REWRITE COM-RECORD.
      CLOSE APCCOM.

             MOVE "S"                TO CRT-TYPE.
             CALL "UTP\CRTHEAD" USING CRT-HEADINGS LS-USER-ID.
             DISPLAY S01.
       AA05.
             PERFORM AC000-PARAM.
             PERFORM AD000-CONTROL.
             PERFORM AE000-DEPART.
             PERFORM AG000-AUDIT.
             PERFORM AF000-TXTRAN.
             PERFORM AH000-CHEQUE.
             PERFORM AI000-SYSUSE.
             GO TO AZ000-EOJ.

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
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "FUNCTION.SCR".

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                      OPT-MESSAGE                          ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ This routine is used to allow the OPERATOR to respond to  ³
      *    ³ a request for an option, so that the correct action can   ³
      *    ³ be performed by the program. The routine will display the ³
      *    ³ message in a pop-up window and allow the OPERATOR to      ³
      *    ³ respond to the 'question'.                                ³
      *    ³                                                           ³
      *    ³ The option request must be placed in WS-OPT-MES and may   ³
      *    ³ not exceed 48 characters. The message will be centred in  ³
      *    ³ the window. An example of a message request follows:      ³
      *    ³                                                           ³
      *    ³   MOVE "Print transactions (Y/N) [ ]" TO WS-OPT-MES.      ³
      *    ³   MOVE Instruction    TO WS-INSTR.                        ³
      *    ³       [see Accptopt for instruction values]               ³
      *    ³   PERFORM OPT-MESSAGE.                                    ³
      *    ³                                                           ³
      *    ³ This would be displayed as:                               ³
      *    ³      ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿   ³
      *    ³      ³          Print transactions (Y/N) [ ]          ³°° ³
      *    ³      ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ°° ³
      *    ³        °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°° ³
      *    ³                                                           ³
      *    ³ The response is returned in WS-OPTION (in upper case).    ³
      *    ³                                                           ³
      *    ³ The system will display the message box with the top line ³
      *    ³ as the value of WS-MES-LINE. If WS-MES-LINE is zero the   ³
      *    ³ default will be 20.                                       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "OPTION.CRT".

      *    *************************************************************
      *    ****   T H I S   R O U T I N E   I S   U S E D   T O
      *    D I S P L A Y   E R R O R   M E S S A G E S
      *    *************************************************************
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                      ERROR-MESSAGE                        ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ To display the error message higher or lower (default is  ³
      *    ³ line 20) Move the line number which must be used as the   ³
      *    ³ top line to WS-MES-LINE. The message to be displayed must ³
      *    ³ be in WS-ERR-MES. PERFORM ERROR-MESSAGE.                  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "ERROR.SCR".

      *
      *     ****   ***  ****   ***  *   *    ***** *** *     *****
      *     *   * *   * *   * *   * ** **    *      *  *     *
      *     *   * *   * *   * *   * * * *    *      *  *     *
      *     ****  ***** ****  ***** *   *    ***    *  *     ***
      *     *     *   * *   * *   * *   *    *      *  *     *
      *     *     *   * *   * *   * *   *    *      *  *     *
      *     *     *   * *   * *   * *   *    *     *** ***** *****
      *
       AC000-PARAM             SECTION.
       AC000-INIT.
           IF LS0-DTP = 2 OR LS0-STP = 2 OR LS0-CRP = 2 OR LS0-GLP = 2 OR LS0-HPD = 2 OR LS0-JCP = 2 OR LS0-VHP = 2
               OPEN I-O PARAM
               DISPLAY "Updated" AT 0633 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
               GO TO AC999-EXIT.
      *
      *    ****   PRINTER CONTROL CHARACTERS (DOT MATRIX)
      *
             MOVE ZERO               TO W01-DEC.
             MOVE W01-CHAR           TO W01-NUL.
             MOVE 27                 TO W01-DEC.
             MOVE W01-CHAR           TO W01-ESC.
             MOVE 15                 TO W01-DEC.
             MOVE W01-CHAR           TO W01-COND.
             MOVE 18                 TO W01-DEC.
             MOVE W01-CHAR           TO W01-NORM.
             MOVE 14                 TO W01-DEC.
             MOVE W01-CHAR           TO W01-EXP.
             MOVE 20                 TO W01-DEC.
             MOVE W01-CHAR           TO W01-ECAN.
             MOVE 69                 TO W01-DEC.
             MOVE W01-CHAR           TO W01-DBLE.
             MOVE 70                 TO W01-DEC.
             MOVE W01-CHAR           TO W01-DCAN.
             MOVE "0"                TO W01-8LIN.
             MOVE "2"                TO W01-6LIN.
             OPEN OUTPUT PARAM.
             MOVE 1                  TO WS-PARKEY.
       AC00.
             MOVE WS-PARKEY          TO WS-NUM.
             DISPLAY WS-NUM AT 0626 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta.

       AC005-LOOP.
             DISPLAY "Company Details" AT 0645 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.
             INITIALIZE PAR-RECORD1.
             MOVE SEC-COMP           TO PAR-COMPANY PAR-CSHEAD.
             MOVE SEC-DATE           TO PAR-DMY.
           IF SEC-PASS = 999999
               MOVE 9                TO PAR-STAT
           ELSE
               MOVE 0                TO PAR-STAT.
             MOVE "A"                TO PAR-STMINV.
             WRITE PAR-RECORD1.
             PERFORM AC00.
             ADD 1                   TO WS-PARKEY.
             INITIALIZE PAR-RECORD2.
             MOVE "( Reg. No.             )"
                                     TO PAR-ADDRESS1.
             MOVE "PO Box/Posbus"    TO PAR-ADDRESS2.
             MOVE "Street Address"   TO PAR-ADDRESS3.
             MOVE "Town/City"        TO PAR-ADDRESS4.
             WRITE PAR-RECORD2.
             PERFORM AC00.
             ADD 1                   TO WS-PARKEY.
             DISPLAY "File Locations " AT 0645 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.
             INITIALIZE PAR-RECORD3.
      *      MOVE ZERO               TO PAR-POS.
             MOVE 7                  TO PAR-INV.
             MOVE 1                  TO PAR-SLIP PAR-CHQ-PRN PAR-CHQ-SPC.
             MOVE 6                  TO PAR-STM.
             MOVE "8.15"             TO PAR-VERSION.
             MOVE "PASSWORD"         TO PAR-PASSWORD.
             MOVE "******"           TO PAR-CSHCODE.
             MOVE "IPRINT"           TO PAR-INVPASS.
             MOVE "Y"                TO PAR-NEW-SYS PAR-PPROMPT PAR-SLIP-P.
             MOVE "N"                TO PAR-OVER-LIM PAR-ARREARS PAR-CASES PAR-PARCELS PAR-SPLIT-BC PAR-ALLOC-BC PAR-ROUND-TOT PAR-USE-3-DEC PAR-INV-CHECK.
             MOVE "X"                TO PAR-BAR-CODE.
             MOVE 80                 TO PAR-BCSIZE.
             MOVE "A"                TO PAR-STOCK.
             MOVE "S"                TO PAR-PLU-D.
             MOVE 14                 TO PAR-SLNGTH.
             MOVE ZERO               TO PAR-SLIP-ROUND PAR-INV-AMT PAR-HFEE.
             MOVE SEC-NUM            TO PAR-SERIAL.
             MOVE SEC-LEV            TO PAR-LEV.
             MOVE SEC-VER            TO PAR-VER.
             MOVE W12-YEAR           TO PAR-CUR-YR.
             MOVE W12-CENT           TO PAR-CUR-CEN PAR-PRV-CEN PAR-NXT-CEN.
             ADD 1                   TO PAR-NXT-CEN.
             SUBTRACT 1              FROM PAR-PRV-CEN.

       AC05A.
             MOVE "Record stock sales on a CARDEX file Y/N  [N]" TO WS-OPT-MES.
             MOVE "N"                TO WS-OPTION.
             MOVE 1                  TO WS-INSTR.
             PERFORM OPT-MESSAGE.
             MOVE WS-OPTION          TO PAR-CRDX.

       AC05B.
             MOVE "Update quantities on a PRICED item Y/N  [N]" TO WS-OPT-MES.
             MOVE "N"                TO WS-OPTION.
             MOVE 1                  TO WS-INSTR.
             PERFORM OPT-MESSAGE.
             MOVE WS-OPTION          TO PAR-PRICED-IND.

       AC05C.
             MOVE "Use the BANK DEPOSIT module Y/N  [N]" TO WS-OPT-MES.
             MOVE "N"                TO WS-OPTION.
             MOVE 1                  TO WS-INSTR.
             PERFORM OPT-MESSAGE.
             MOVE WS-OPTION          TO PAR-BANK-IND.
       AC05D.
             MOVE "Use PROMPTS with sales Y/N  [Y]" TO WS-OPT-MES.
             MOVE "Y"                TO WS-OPTION.
             MOVE 1                  TO WS-INSTR.
             PERFORM OPT-MESSAGE.
             MOVE WS-OPTION          TO PAR-PROMPT.
             PERFORM CLEAR-L50.
             WRITE PAR-RECORD3.
             PERFORM AC00.
             ADD 1                   TO WS-PARKEY.
             INITIALIZE PAR-RECORD4.
             DISPLAY "Operator Details " AT 0645 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.
             MOVE SEC-TERMS          TO PAR-TRMS.
             MOVE "N"                TO PAR-AGE.
             WRITE PAR-RECORD4.
             PERFORM AC00.
             ADD 1                   TO WS-PARKEY.
             MOVE "DS"               TO PAR-SYS.
             DISPLAY "Debtor Discount Codes" AT 0645 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.
       AC06.
             INITIALIZE PAR-RECORD5.
             MOVE 0                  TO WS-S1
             PERFORM TEST BEFORE UNTIL WS-S1 = 9
                     ADD 1           TO WS-S1
                     MOVE "APACPW"   TO PAR-SUPER(WS-S1)
             END-PERFORM.
             MOVE 28.00              TO PAR-MARG.
             MOVE 5.00               TO PAR-ADV-RATE.
             MOVE 1.75               TO PAR-FIN-RATE.
             MOVE 30                 TO PAR-PERIOD.
             WRITE PAR-RECORD5.
             PERFORM AC00.
             DISPLAY "Purchase Journal     " AT 0645 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.
             ADD 1                   TO WS-PARKEY.
             INITIALIZE PAR-RECORD6.
             MOVE "QWERTYUIOP"       TO PAR-COST-CODE.
             MOVE 62                 TO PAR-LPP.
             MOVE "N"                TO PAR-WHOLESALE PAR-INTEGRATE.
             MOVE 1                  TO PAR-DEBGL PAR-CREGL PAR-INTGL PAR-BNKGL PAR-UNPROF PAR-REDGL PAR-ADJGL PAR-RLGL PAR-DSGL.
             MOVE 1                  TO PAR-TRF-REF.
             MOVE 34                 TO PAR-STM-DET.
             WRITE PAR-RECORD6.
             PERFORM AC00.
             ADD 1                   TO WS-PARKEY.
             DISPLAY "Cash Sale - control  " AT 0645 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.
             INITIALIZE PAR-RECORD7.
             MOVE 1                  TO PAR-PIC-REF.
             WRITE PAR-RECORD7.
             MOVE "Y"                TO PAR-CSH-ADD PAR-CSH-TEL PAR-CSH-VAT PAR-CSH-TIME PAR-CSH-ASSIST.
             MOVE "2"                TO PAR-CSH-DETAIL.
             PERFORM AC00.
             ADD 1                   TO WS-PARKEY.
             DISPLAY "Cost of Sales        " AT 0645 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.
             INITIALIZE PAR-RECORD8.
             MOVE "Description"      TO PAR-DESC-H1.
             MOVE "Description 2"    TO PAR-DESC-H2.
             MOVE "-Wholesale"       TO PAR-WS-HD.
             MOVE "-Cash     "       TO PAR-CS-HD.
             MOVE "-Retail   "       TO PAR-RT-HD.
             WRITE PAR-RECORD8.
             PERFORM AC00.
             ADD 1                   TO WS-PARKEY.
             DISPLAY "VAT Control Account  " AT 0645 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.
             INITIALIZE PAR-RECORD9.
             MOVE "V"                TO PAR-VAT-GST.
             WRITE PAR-RECORD9.
             PERFORM AC00.
             ADD 1                   TO WS-PARKEY.
             DISPLAY "°°°°°°°°°°°°°°°°°°°°°" AT 0645 WITH BACKGROUND-COLOR Blue HIGHLIGHT FOREGROUND-COLOR Cyan.
             MOVE 1                  TO WS-S1.

       AC08.
             MOVE 1                  TO PAR-T-PRN1(WS-S1).
             MOVE 2                  TO PAR-T-PRN2(WS-S1).
           IF WS-S1 < 63
               ADD 1                 TO WS-S1
               GO TO AC08.
             WRITE PAR-RECORD10.
             PERFORM AC00.
             ADD 1                   TO WS-PARKEY.
             DISPLAY "Transaction Codes" AT 0645 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.
       AC09.
             INITIALIZE PAR-RECORD11.

       AC010-LOOP.
           IF WS-PARKEY = 11
               MOVE 01               TO PAR-T-CODE(1)
               MOVE 02               TO PAR-T-CODE(2)
               MOVE "Invoice"        TO PAR-E-DESC(1)
               MOVE "Faktuur"        TO PAR-A-DESC(1)
               MOVE "Receipt"        TO PAR-E-DESC(2)
               MOVE "Kwitansie"      TO PAR-A-DESC(2)
               MOVE 0                TO PAR-ACTION(1)
               MOVE 1                TO PAR-ACTION(2).
           IF WS-PARKEY = 12
               MOVE 03               TO PAR-T-CODE(1)
               MOVE 04               TO PAR-T-CODE(2)
               MOVE "Debit Note "    TO PAR-E-DESC(1)
               MOVE "Debiet Nota"    TO PAR-A-DESC(1)
               MOVE "Credit Note"    TO PAR-E-DESC(2)
               MOVE "Krediet Nota"   TO PAR-A-DESC(2)
               MOVE 0                TO PAR-ACTION(1)
               MOVE 1                TO PAR-ACTION(2).
           IF WS-PARKEY = 13
               MOVE 05               TO PAR-T-CODE(1)
               MOVE 06               TO PAR-T-CODE(2)
               MOVE "Dt Journal"     TO PAR-E-DESC(1)
               MOVE "Dt Joernaal"    TO PAR-A-DESC(1)
               MOVE "Cr Journal"     TO PAR-E-DESC(2)
               MOVE "Kt Joernaal"    TO PAR-A-DESC(2)
               MOVE 0                TO PAR-ACTION(1)
               MOVE 1                TO PAR-ACTION(2).
           IF WS-PARKEY = 17
               MOVE 13               TO PAR-T-CODE(1)
               MOVE 14               TO PAR-T-CODE(2)
               MOVE "Tax Refund"     TO PAR-E-DESC(1)
               MOVE "Tax"            TO PAR-E-DESC(2)
               MOVE "Belasting"      TO PAR-A-DESC(1)
               MOVE "Belasting"      TO PAR-A-DESC(2)
               MOVE 0                TO PAR-ACTION(1)
               MOVE 1                TO PAR-ACTION(2).
           IF WS-PARKEY = 18
               MOVE 15               TO PAR-T-CODE(1)
               MOVE 16               TO PAR-T-CODE(2)
              MOVE "Refund"          TO PAR-E-DESC(1)
               MOVE "Chq Reversal"   TO PAR-E-DESC(2)
               MOVE "Tjek Betaal"    TO PAR-A-DESC(1)
               MOVE "Tjek Terug"     TO PAR-A-DESC(2)
               MOVE 0                TO PAR-ACTION(1)
               MOVE 0                TO PAR-ACTION(2).
           IF WS-PARKEY = 19
               MOVE 17               TO PAR-T-CODE(1)
               MOVE 18               TO PAR-T-CODE(2)
               MOVE "Int Reversed"   TO PAR-E-DESC(1)
               MOVE "Instalment"     TO PAR-E-DESC(2)
               MOVE "Rente Terug"    TO PAR-A-DESC(1)
               MOVE "Paaiment"       TO PAR-A-DESC(2)
               MOVE 1                TO PAR-ACTION(1)
               MOVE 0                TO PAR-ACTION(2).
           IF WS-PARKEY = 20
               MOVE 19               TO PAR-T-CODE(1)
               MOVE 20               TO PAR-T-CODE(2)
               MOVE "Discount"       TO PAR-E-DESC(1)
               MOVE "Afslag"         TO PAR-A-DESC(1)
               MOVE "Int Arrears"    TO PAR-E-DESC(2)
               MOVE "Rnt Agterst"    TO PAR-A-DESC(2)
               MOVE 1                TO PAR-ACTION(1)
               MOVE 0                TO PAR-ACTION(2).
             WRITE PAR-RECORD11.
             PERFORM AC00.
             PERFORM AC09.
             ADD 1                   TO WS-PARKEY
           IF WS-PARKEY < 61
               GO TO AC010-LOOP.
             INITIALIZE PAR-RECORD61.

       AC15.
             WRITE PAR-RECORD61.
             PERFORM AC00.
             ADD 1                   TO WS-PARKEY.
           IF WS-PARKEY < 101
              GO TO AC15.

       AC20.
             INITIALIZE PAR-RECORD101.
             DISPLAY "Salesman Details " AT 0645 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.

       AC40.
             WRITE PAR-RECORD101.
             PERFORM AC00.
             ADD 1                   TO WS-PARKEY
           IF WS-PARKEY < 151
               GO TO AC40.
             DISPLAY "Cash Draw Details" AT 0645 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.
             INITIALIZE PAR-RECORD151.
             MOVE ZERO               TO PAR-CSHDTE PAR-TAXVAL PAR-FLOAT PAR-CPRN PAR-RECEIPTS PAR-CUST    
                                        PAR-CSHMTD PAR-VATVAL PAR-CARDS PAR-CASH PAR-BANKTRF  PAR-CSH        
                                        PAR-CSHYTD PAR-PETTY  PAR-SPEED PAR-ITMS PAR-CHEQUES  PAR-VOUCHERS.
             MOVE "N"                TO PAR-DRAWER PAR-USE-3  PAR-SCALE PAR-CUSDISP PAR-KTCHPRN.
             MOVE 1                  TO PAR-CUSPORT PAR-SCALPORT PAR-KCHPORT PAR-PORT.
             MOVE SPACES             TO PAR-KICK PAR-SCALE-TYPE.
             MOVE W01-NUL            TO PAR-NUL.
             MOVE W01-ESC            TO PAR-ESC.
             MOVE W01-DBLE           TO PAR-DBL.
             MOVE W01-DCAN           TO PAR-CDBL.
             MOVE W01-COND           TO PAR-SI.
             MOVE W01-NORM           TO PAR-CSI.
             MOVE W01-EXP            TO PAR-SO.
             MOVE W01-ECAN           TO PAR-CSO.
             MOVE W01-6LIN           TO PAR-6LPI.
             MOVE W01-8LIN           TO PAR-8LPI.
             MOVE 1                  TO PAR-CPRN.
             MOVE 6                  TO PAR-ADVANCE.

       AC45.
             WRITE PAR-RECORD151.
             PERFORM AC00.
             ADD 1                   TO WS-PARKEY
           IF WS-PARKEY < 191
               GO TO AC45.
             DISPLAY "Creditor Tr codes" AT 0645 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.

       AC50.
             INITIALIZE PAR-RECORD191.

       AC55.
           IF WS-PARKEY = 191
               MOVE "Invoice"        TO PAR-C-DESC(1)
               MOVE "Inv"            TO PAR-C-ABR(1)
               MOVE 0                TO PAR-C-ACT(1)
               MOVE 01               TO PAR-C-CODE(1)
               MOVE "Debit Note"     TO PAR-C-DESC(2)
               MOVE "D/N"            TO PAR-C-ABR(2)
               MOVE 1                TO PAR-C-ACT(2)
               MOVE 02               TO PAR-C-CODE(2).
           IF WS-PARKEY = 192
               MOVE "Cheque"         TO PAR-C-DESC(1)
               MOVE "Chq"            TO PAR-C-ABR(1)
               MOVE 1                TO PAR-C-ACT(1)
               MOVE 03               TO PAR-C-CODE(1)
               MOVE "Payment"        TO PAR-C-DESC(2)
               MOVE "Pmt"            TO PAR-C-ABR(2)
               MOVE 0                TO PAR-C-ACT(2)
               MOVE 04               TO PAR-C-CODE(2).
           IF WS-PARKEY = 193
               MOVE "Debit Transfer" TO PAR-C-DESC(1)
               MOVE "Trf"            TO PAR-C-ABR(1)
               MOVE 1                TO PAR-C-ACT(1)
               MOVE 05               TO PAR-C-CODE(1)
               MOVE "Credit Transfer" TO PAR-C-DESC(2)
               MOVE "Trf"            TO PAR-C-ABR(2)
               MOVE 0                TO PAR-C-ACT(2)
               MOVE 06               TO PAR-C-CODE(2).
           IF WS-PARKEY = 194
               MOVE "Discount"       TO PAR-C-DESC(2)
               MOVE "Dsc"            TO PAR-C-ABR(2)
               MOVE 1                TO PAR-C-ACT(2)
               MOVE 08               TO PAR-C-CODE(2).
           IF WS-PARKEY = 195
               MOVE "Debit Journal"  TO PAR-C-DESC(1)
               MOVE "D/J"            TO PAR-C-ABR(1)
               MOVE 1                TO PAR-C-ACT(1)
               MOVE 09               TO PAR-C-CODE(1)
               MOVE "Credit Journal" TO PAR-C-DESC(2)
               MOVE "C/J"            TO PAR-C-ABR(2)
               MOVE 0                TO PAR-C-ACT(2)
               MOVE 10               TO PAR-C-CODE(2).
           IF WS-PARKEY = 196
               MOVE "Invoice (G/L)"  TO PAR-C-DESC(1)
               MOVE "Inv"            TO PAR-C-ABR(1)
               MOVE 0                TO PAR-C-ACT(1)
               MOVE 11               TO PAR-C-CODE(1)
               MOVE "Debit Note (G/L)" TO PAR-C-DESC(2)
               MOVE "D/N"            TO PAR-C-ABR(2)
               MOVE 1                TO PAR-C-ACT(2)
               MOVE 12               TO PAR-C-CODE(2).
           IF WS-PARKEY = 197
               MOVE "Payment (G/L)"  TO PAR-C-DESC(2)
               MOVE "Pmt"            TO PAR-C-ABR(2)
               MOVE 0                TO PAR-C-ACT(2)
               MOVE 14               TO PAR-C-CODE(2).
             WRITE PAR-RECORD191.
             PERFORM AC00.
             ADD 1                   TO WS-PARKEY
           IF WS-PARKEY < 201
               MOVE 1                TO WS-S1
               GO TO AC50.
             DISPLAY "Technician details" AT 0645 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.
             INITIALIZE PAR-RECORD201.

       AC60.
             WRITE PAR-RECORD201.
             PERFORM AC00.
             ADD 1                   TO WS-PARKEY
           IF WS-PARKEY < 251
               GO TO AC60.
             DISPLAY "Pinter specifications" AT 0645 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.
             INITIALIZE PAR-RECORD251.
             MOVE SEC-PRNCOND        TO W10-CONP.
             MOVE SEC-PRNNORM        TO W10-NORP.
             MOVE SEC-PRNEXPD        TO W10-EXPP.
             MOVE SEC-PRNECAN        TO W10-ECAN.
             MOVE SEC-PAGE48         TO W10-8LPI.
             MOVE SEC-PAGE66         TO W10-6LPI.
             MOVE SEC-10CPI          TO W10-10CPI.
             MOVE SEC-12CPI          TO W10-12CPI.
             MOVE SEC-17CPI          TO W10-17CPI.
             MOVE W09-CONP           TO TRM-CONP(1)   TRM-CONP(2).
             MOVE W09-NORP           TO TRM-NORP(1)   TRM-NORP(2).
             MOVE W09-EXPP           TO TRM-EXPP(1)   TRM-EXPP(2).
             MOVE W09-ECAN           TO TRM-ECAN(1)   TRM-ECAN(2).
             MOVE W09-8LPI           TO TRM-8LPI(1)   TRM-8LPI(2).
             MOVE W09-6LPI           TO TRM-6LPI(1)   TRM-6LPI(2).
             MOVE W09-10CPI          TO TRM-10CPI(1)  TRM-10CPI(2).
             MOVE W09-12CPI          TO TRM-12CPI(1)  TRM-12CPI(2).
             MOVE W09-17CPI          TO TRM-17CPI(1)  TRM-17CPI(2).
             MOVE 66                 TO TRM-PAGE(1)   TRM-PAGE(2).
             MOVE 62                 TO TRM-LENGTH(1) TRM-LENGTH(2).
             MOVE "6"                TO TRM-VERSION.

       AC65.
             WRITE PAR-RECORD251.
             PERFORM AC00.
             ADD 1                   TO WS-PARKEY
           IF WS-PARKEY < 255
               GO TO AC65.
             DISPLAY "Terminal printers    " AT 0645 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.
             INITIALIZE PAR-RECORD255.
             MOVE 0                  TO WS-S1.
             PERFORM TEST AFTER UNTIL WS-S1 = 10
                     ADD 1           TO WS-S1
                     MOVE 1          TO TRM-PICP(WS-S1) TRM-PRN1(WS-S1)
                                        TRM-INVP(WS-S1) TRM-PRN2(WS-S1)
                                        TRM-STDP(WS-S1) TRM-PRN3(WS-S1)
                                        TRM-STMP(WS-S1) TRM-PRN4(WS-S1)
                                        TRM-CSHP(WS-S1) TRM-PRN5(WS-S1)
                                        TRM-POS (WS-S1) TRM-PRN6(WS-S1)
             END-PERFORM.
             PERFORM TEST BEFORE UNTIL WS-PARKEY = 266
                     WRITE PAR-RECORD255
                     PERFORM AC00
                     ADD 1           TO WS-PARKEY
             END-PERFORM.
             DISPLAY "Quotation Remarks    " AT 0645 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.
             INITIALIZE PAR-RECORD266.
             PERFORM TEST BEFORE UNTIL WS-PARKEY = 275
                     WRITE PAR-RECORD266
                     PERFORM AC00
                     ADD 1           TO WS-PARKEY
             END-PERFORM.
             MOVE "C"                TO PAR-QTE-FMT.
             MOVE 6                  TO PAR-QTE-MRG.
             MOVE "Dear Sir/Madam,"  TO PAR-QTE-SAL.
             MOVE "Yours sincerely"  TO PAR-QTE-SIG.
             WRITE PAR-RECORD275.
             ADD 1                   TO WS-PARKEY
             INITIALIZE PAR-RECORD276.
             PERFORM TEST BEFORE UNTIL WS-PARKEY = 285
                     WRITE PAR-RECORD276
                     PERFORM AC00
                     ADD 1           TO WS-PARKEY
             END-PERFORM.
             DISPLAY "°°°°°°°°°°°°°°°°°°°°°" AT 0645 WITH BACKGROUND-COLOR Blue HIGHLIGHT FOREGROUND-COLOR Cyan.
             DISPLAY WS-MES AT 0633 WITH FOREGROUND-COLOR Brown HIGHLIGHT BACKGROUND-COLOR Magenta.

       AC999-EXIT.
             EXIT.
      *
      *          ***   ***  *   * ***** ****   ***  *             *
      *         *   * *   * *   *   *   *   * *   * *            *
      *         *     *   * **  *   *   *   * *   * *           *
      *         *     *   * * * *   *   ****  *   * *          *
      *         *     *   * *  **   *   *   * *   * *         *
      *         *   * *   * *   *   *   *   * *   * *        *
      *          ***   ***  *   *   *   *   *  ***  *****   *
      *
      *  *   * ***** ***** *   *  ***  ****  *   *     ***** *** *     *****
      *  *   * *       *   *   * *   * *   * *  *      *      *  *     *
      *  **  * *       *   *   * *   * *   * * *       *      *  *     *
      *  * * * ***     *   * * * *   * ****  **        ***    *  *     ***
      *  *  ** *       *   * * * *   * *   * * *       *      *  *     *
      *  *   * *       *   * * * *   * *   * *  *      *      *  *     *
      *  *   * *****   *    * *   ***  *   * *   *     *     *** ***** *****
      *
       AD000-CONTROL           SECTION.
       AD000-INIT.
          IF LS0-DTP = 2 OR LS0-STP = 2 OR LS0-CRP = 2 OR LS0-GLP = 2
               OPEN I-O NETWORK
               GO TO AD30
           ELSE
               OPEN OUTPUT NETWORK.
             MOVE 1                  TO WS-NETKEY.
             INITIALIZE NET-DEBTOR.
             MOVE 1                  TO DEB-BATCH DEB-INVOICE DEB-ORDER DEB-RECEIPT DEB-VOUCH DEB-JOBNO.
             MOVE 2                  TO DEB-AUDIT.
             WRITE NET-DEBTOR.

       AD05.
             MOVE WS-NETKEY          TO WS-NUM.
             DISPLAY WS-NUM AT 0826 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta.

       AD10.
             ADD 1                   TO WS-NETKEY.
             INITIALIZE NET-CREDITOR.
             MOVE 1                  TO P-CRTRAN P-PURORD.
             MOVE 2                  TO P-CREDIT P-CREMAX.
             MOVE 68                 TO P-CINDEX P-PURIDX.
             WRITE NET-CREDITOR.
             PERFORM AD05.

       AD15.
             ADD 1                   TO WS-NETKEY.
             INITIALIZE NET-STOCK.
             MOVE "N"                TO STK-CARDEX.
             MOVE "PASSWORD"         TO STK-PASSWORD.
             WRITE NET-STOCK.
             PERFORM AD05.

       AD20.
             ADD 1                   TO WS-NETKEY.
             INITIALIZE NET-LEDGER.
             MOVE 2                  TO G-OPEN.
             MOVE 1                  TO G-GLINST G-BATCH.
             WRITE NET-LEDGER.
             PERFORM AD05.

       AD25.
             ADD 1                   TO WS-NETKEY.
             INITIALIZE NET-LAYBUY.
             MOVE 2                  TO LAY-LAYBUY LAY-LAYMAX.
             MOVE 68                 TO LAY-AINDEX.
             WRITE NET-LAYBUY.
             PERFORM AD05.

       AD30.
             DISPLAY WS-MES AT 0833 WITH FOREGROUND-COLOR Brown HIGHLIGHT BACKGROUND-COLOR Magenta.

       AD999-EXIT.
             EXIT.
      *
      *    ****  ***** ****    *   ****  *****    ***** *** *     *****
      *    *   * *     *   *  * *  *   *   *      *      *  *     *
      *    *   * *     *   * *   * *   *   *      *      *  *     *
      *    *   * ***   ****  ***** ****    *      ***    *  *     ***
      *    *   * *     *     *   * *  *    *      *      *  *     *
      *    *   * *     *     *   * *   *   *      *      *  *     *
      *    ****  ***** *     *   * *   *   *      *     *** ***** *****
      *
       AE000-DEPART    SECTION.
       AE00.
           IF LS0-DTP = 2 OR LS0-STP = 2 OR LS0-CRP = 2 OR LS0-GLP = 2
               OPEN I-O DEPART
               GO TO AE15.
             OPEN OUTPUT DEPART.
             INITIALIZE DPT-RECORD.
             MOVE "VAT1"             TO DPT-CODE.
             MOVE "VALUE ADDED TAX"  TO DPT-DESC.
             MOVE 19930407           TO DPT-R-DATE.
             MOVE 14.00              TO DPT-RATE.
             MOVE 10.00              TO DPT-P-RATE.

       AE10.
             DISPLAY DPT-CODE AT 1026 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta.
             WRITE DPT-RECORD.

       AE12.
             MOVE "VAT2"             TO DPT-CODE.
             PERFORM AE10.
             MOVE "VAT3"             TO DPT-CODE
             PERFORM AE10.
             MOVE "VAT4"             TO DPT-CODE
             PERFORM AE10.
             MOVE "VAT5"             TO DPT-CODE
             PERFORM AE10.
             MOVE "VAT6"             TO DPT-CODE
             PERFORM AE10.
             MOVE ZERO               TO DPT-R-DATE DPT-RATE DPT-P-RATE.
             MOVE "XXXX"             TO DPT-CODE.
             MOVE "UNSPECIFIED SALES" TO DPT-DESC.
             PERFORM AE10.
             MOVE "YYYY"             TO DPT-CODE.
             MOVE "SETTLEMENT DISCOUNT GIVEN" TO DPT-DESC.
             PERFORM AE10.
             CLOSE DEPART.
             OPEN I-O DEPART.

       AE15.
             DISPLAY WS-MES AT 1033 WITH FOREGROUND-COLOR Brown HIGHLIGHT BACKGROUND-COLOR Magenta.

       AE999.
             EXIT.
      *
      *  ***** *   * ***** ****    *   *   *     **** *** *    ****
      *    *   *   *   *   *   *  * *  *   *     *     *  *    *
      *    *    * *    *   *   * *   * **  *     *     *  *    *
      *    *     *     *   ****  ***** * * *     ***   *  *    ***
      *    *    * *    *   *   * *   * *  **     *     *  *    *
      *    *   *   *   *   *   * *   * *   *     *     *  *    *
      *    *   *   *   *   *   * *   * *   *     *    *** **** ****
      *
       AF000-TXTRAN            SECTION.
       AF000-INIT.
          IF LS0-DTP = 2 OR LS0-STP = 2 OR LS0-CRP = 2 OR LS0-GLP = 2
              OPEN I-O TXTRAN
              GO TO AF05.
             OPEN OUTPUT TXTRAN.
             INITIALIZE TAX-RECORD1.
             MOVE "0"                TO TAX-TYPE.
             MOVE 0                  TO TAX-ACTYPE.
             MOVE ZERO               TO TAX-DATE.
             WRITE TAX-RECORD1.

       AF05.
             DISPLAY WS-MES AT 1433 WITH FOREGROUND-COLOR Brown HIGHLIGHT BACKGROUND-COLOR Magenta.

       AF999-EXIT.
             EXIT.
      *
      *      *   *   * ****  *** *****     ***** *** *     *****
      *     * *  *   * *   *  *    *       *      *  *     *
      *    *   * *   * *   *  *    *       *      *  *     *
      *    ***** *   * *   *  *    *       ***    *  *     ***
      *    *   * *   * *   *  *    *       *      *  *     *
      *    *   * *   * *   *  *    *       *      *  *     *
      *    *   *  ***  ****  ***   *       *     *** ***** *****
      *
       AG000-AUDIT             SECTION.
       AG000-INIT.
          IF LS0-DTP = 2 OR LS0-STP = 2 OR LS0-CRP = 2 OR LS0-GLP = 2
              OPEN I-O AUDIT
              GO TO AG05.
             OPEN OUTPUT AUDIT.
             MOVE 1                  TO WS-AUDKEY.
             INITIALIZE AUD-HDR.
             WRITE AUD-REC1.

       AG05.
             DISPLAY WS-MES AT 1233 WITH FOREGROUND-COLOR Brown HIGHLIGHT BACKGROUND-COLOR Magenta.

       AG999-EXIT.
             EXIT.
      /
       AH000-CHEQUE            SECTION.
       AH000-INIT.
             OPEN OUTPUT CHEQUE.
             DISPLAY WS-MES AT 1633 WITH FOREGROUND-COLOR Brown HIGHLIGHT BACKGROUND-COLOR Magenta.

       AH999-EXIT.
             EXIT.
      /
       AI000-SYSUSE        SECTION.
       AI000-INIT.
             OPEN OUTPUT SYSUSER.
             INITIALIZE USE-RECORD.
             MOVE 001                TO USE-NO.
             MOVE "MISTTO                R RANIAD" TO USE-NAME.
             MOVE "oNsjyzuy"         TO USE-PASS.
             MOVE "Y"                TO USE-GL USE-STOCK USE-DEBT USE-CRED USE-PARM USE-SALES USE-JOBS USE-WAGES.
             MOVE 9                  TO USE-GLLEV USE-STLEV USE-DBLEV USE-CRLEV USE-PALEV USE-SALEV USE-JBLEV USE-WGLEV.
             WRITE USE-RECORD.
             DISPLAY WS-MES AT 1833 WITH FOREGROUND-COLOR Brown HIGHLIGHT BACKGROUND-COLOR Magenta.

       AI999-EXIT.
             EXIT.
      /
       AZ000-END               SECTION.
       AZ000-EOJ.
           IF SEC-PASS = 999999
               MOVE 979695           TO SEC-PASS.
             REWRITE SEC-REC1.
             CLOSE SECUR.
             CLOSE CHEQUE.
             CLOSE PARAM.
             CLOSE NETWORK.
             CLOSE DEPART.
             CLOSE AUDIT.
             CLOSE SYSUSER.
             CLOSE TXTRAN.

       AZ02.
             DISPLAY "***** Initialization Complete ****" AT 2212 WITH FOREGROUND-COLOR Brown HIGHLIGHT.

       AZ05.
             PERFORM ERASE-SCREEN.
           IF LS0-DTP = 1
               DISPLAY "Loading Debtor initialize program" AT 1212
               MOVE "UTP\DTP000"     TO LS-NEXT-PRG
               GO TO AZ999.
           IF LS0-JCP = 1
               DISPLAY "Loading Job Costing initialize program" AT 1212
               MOVE "UTP\JCP000"     TO LS-NEXT-PRG
               GO TO AZ999.
           IF LS0-STP = 1
               DISPLAY "Loading Stock initialize program" AT 1212
               MOVE "UTP\STP000"     TO LS-NEXT-PRG
               GO TO AZ999.
           IF LS0-CRP = 1
               DISPLAY "Loading Creditors initialize program" AT 1212
               MOVE "UTP\CRP000"     TO LS-NEXT-PRG
               GO TO AZ999.
           IF LS0-GLP = 1
               DISPLAY "Loading G/Ledger initialize program" AT 1212
               MOVE "UTP\GLP000"     TO LS-NEXT-PRG
               GO TO AZ999.
             MOVE SPACES             TO LS-NEXT-PRG.

       AZ999.
             EXIT PROGRAM.
