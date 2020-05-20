      $SET LINKCOUNT"320" GNT"CRP027.GNT"
      ******************************************************************
      *                                                                *
      *    ******   *******   *******     ****     ******   ********   *
      *   **    **  **    **  **    **   **  **   **    **       **    *
      *   **        **    **  **    **  **    **       **       **     *
      *   **        ******    *******   **    **     **        **      *
      *   **        **   **   **        **    **    **        **       *
      *   **    **  **    **  **         **  **    **         **       *
      *    ******   **    **  **          ****    ********    **       *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *  C R E D I T O R   A D D R E S S   L A B E L S   P R O G R A M *
      *                                                                *
      *       Version 9.04.05 - June 2018                              *
      *                                                                *
      ******************************************************************
      *                                                                *
      * June 2018 - Added program CRPRDWR to handle IO for Creditor    *
      *             file and COPY file CA155.CRP to call and handlw IO *
      *             for the CREDITOR file. SEE CA155.CRP and CRPRDWR   *
      *             for details.                                       *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     CRP027.
       AUTHOR.         J W LEMMON (APAC).
       DATE-WRITTEN.   OCTOBER 1982.

                COPYRIGHT NOTICE: COPYRIGHT (C) 1982 - 2017
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

       COPY "CREDIT.SL".

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

       COPY "CREDIT.FDE".

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
       77  WS-IXS1                 PIC  9(04)    COMP-5.
       77  WS-IXS2                 PIC  9(04)    COMP-5.
       77  WS-IXS3                 PIC  9(04)    COMP-5.
       77  WS-IXS4                 PIC  9(04)    COMP-5.
       77  WS-PARKEY               PIC  9(04)    COMP-5.
       77  WS-NETKEY               PIC  9(06)    COMP-5.
       77  WS-NTP                  PIC  9(01).
       77  WS-LINES                PIC  9(02).
       77  WS-PAGE                 PIC  9(04)    COMP-5.
       77  WS-OPTION               PIC  X(01).
       77  WS-SEQ                  PIC  X(01).
       77  WS-CREDIT               PIC  X(01).
       77  WS-ZERO                 PIC  X(01).
       77  WS-NUM                  PIC  Z9.
       77  WS-ADDR                 PIC  X(01).
       77  WS-ERROR                PIC  9(01).
       77  WS-ERROR1               PIC  9(01).
       77  WS-PGE-LENGTH           PIC  9(02)           VALUE 66.
       77  WS-PRN-LENGTH           PIC  9(02)           VALUE 62.
       77  WS-COMP                 PIC  9(01).
       77  WS-S1                   PIC  9(04)    COMP-5.
       77  WS-S2                   PIC  9(04)    COMP-5.
       77  WS-S3                   PIC  9(04)    COMP-5.
       77  WS-S4                   PIC  9(04)    COMP-5.
       77  WS-S5                   PIC  9(04)    COMP-5.
       77  WS-S6                   PIC  9(04)    COMP-5.
       77  WS-S7                   PIC  9(04)    COMP-5.
       77  WS-S8                   PIC  9(04)    COMP-5.
       77  TODAY-DDMMYY            PIC  9(08)    COMP-5.
       77  WS-USUB                 PIC  9(04)    COMP-5.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Used by calling program (WORKING-STORAGE) and called program ³
      *    ³ (LINKAGE SECTION) for Main screen layout and headings.       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       01  CRT-HEADINGS.
      *
      *    TYPE:
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
           03  CRT-START           PIC  9(02).
           03  CRT-END             PIC  9(02).
      *
      *    Calling Program
      *
           03  CRT-PROGRAM         PIC  X(15)           VALUE "CRP027".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40)           VALUE "CREDITOR - ADDRESS LABELS".
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

       01  W00-TRAN-CODES.
           03  W00-TRAN-TYPES                    OCCURS 100.
               05  W00-TRAN-CODE   PIC  9(02)    COMP-5.
               05  W00-ACTION      PIC  9(02)    COMP-5.
               05  W00-E-DESC      PIC  X(12).
               05  W00-A-DESC      PIC  X(12).

       01  WS-PARID.
           03  WS-SYS-ID           PIC  X(03).

       01  W02-FID.

       COPY "CONTROL.ID".

       COPY "CREDIT.ID".

       COPY "PARAM.ID".

       01  W02-PRINTER-DETAILS.
           03  W02-PRINTER         PIC  X(12).
           03  W02-PGE-LENGTH      PIC  9(03).
           03  W02-PRN-LENGTH      PIC  9(03).
           03  W02-LINAGE          PIC  9(03).
           03  W02-PRN-STATUS      PIC  X(01)           VALUE "C".
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

           03  R-L1                              REDEFINES W02-PRN-LINE.
               05  R-DET           PIC  X(140).

           03  LAB-L1                            REDEFINES W02-PRN-LINE.
               05  LAB-DETAIL                    OCCURS 2.
                   07  FILLER      PIC  X(02).
                   07  LAB-DET     PIC  X(36).
                   07  FILLER      PIC  X(02).

       01  W10-SPARES.
          03  W10-DATE        PIC X(10).
           03  W10-RDATE REDEFINES W10-DATE.
        05  W10-DTE     PIC 99/99/9999.

       COPY "W12.WS".

       01  W15-DATES.
          03  W15-START       PIC  9(08).
           03  W15-SDTE-DMY REDEFINES W15-START.
              05  W15-SDAY    PIC  9(02).
              05  W15-SMNTH   PIC  9(02).
        05  W15-SCENT   PIC  9(02).
              05  W15-SYEAR   PIC  9(02).
          03  W15-S-YMD       PIC  9(08).
           03  W15-STRT-YMD REDEFINES W15-S-YMD.
        05  W15-SCC     PIC  9(02).
              05  W15-SYY     PIC  9(02).
              05  W15-SMM     PIC  9(02).
              05  W15-SDD     PIC  9(02).
          03  W15-END        PIC  9(08).
           03  W15-EDTE-DMY REDEFINES W15-END.
              05  W15-EDAY    PIC  9(02).
              05  W15-EMNTH   PIC  9(02).
        05  W15-ECENT   PIC  9(02).
              05  W15-EYEAR   PIC  9(02).
          03  W15-E-YMD       PIC  9(08).
           03  W15-END-YMD REDEFINES W15-E-YMD.
        05  W15-ECC     PIC  9(02).
              05  W15-EYY     PIC  9(02).
              05  W15-EMM     PIC  9(02).
              05  W15-EDD     PIC  9(02).

       01  W20-DATE.
    03  W20-CC.
        05  W20-CENT    PIC  9(02).
           03  W20-YY.
              05  W20-YEAR    PIC  9(02).
           03  W20-MM.
              05  W20-MONTH   PIC  9(02).
           03  W20-DD.
              05  W20-DAY     PIC  9(02).
       01  W20-DTE1 REDEFINES W20-DATE.
          03  W20-DTE        PIC  9(08).

       01  W25-CALCS.
           03  W25-RESULT      PIC 9(05)V99.
           03  W25-RESULT1 REDEFINES W25-RESULT.
               05  W25-WHOLE   PIC 9(05).
               05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY    PIC 9(04).
               05  W25-SUB    PIC 9(01).
               05  FILLER     PIC 9(02).
           03  W25-TOTAL       PIC S9(07)V99 COMP-3.
           03  W25-VALUE       PIC S9(07)V99 COMP-3.

       01  W40-COMPANY.
           02  W40-NAME1.
               03  W40-NAME.
                   05  W40-ENTRY PIC X(07).
                   05  FILLER  PIC X(33).
               03  FILLER      PIC X(32).
           02  W40-NAME2 REDEFINES W40-NAME1.
               03  W40-SURNAME PIC X(24).
               03  W40-FNAME   PIC X(24).
               03  W40-SNAME   PIC X(24).
           02  W40-NAME3 REDEFINES W40-NAME1.
               03  W40-NCHAR OCCURS 72 PIC X(01).
           02  W40-ADDRESS1.
               03  W40-ADD1    PIC X(30).
               03  W40-ADD2    PIC X(30).
               03  W40-ADD3    PIC X(30).
               03  W40-ADD4    PIC X(30).
           02  W40-ADDRESS2 REDEFINES W40-ADDRESS1.
               03  W40-ACHAR OCCURS 120 PIC X(01).
           02  W40-POST        PIC 9(04).
           02  W40-TEL         PIC X(14).
           02  W40-HTEL        PIC X(14).
           02  W40-TAX REDEFINES W40-HTEL.
               03  W40-TAXNO   PIC 9(14)     COMP-3.
           02  W40-TITLE       PIC 9(04)     COMP-5.
           02  W40-LANG        PIC X(01).

       COPY "W40.WS".

      *
      *    STORAGE FOR PRINTING OF LABELS
      *
       01  W46-LABELS.
           03  W46-LABEL OCCURS 2.
               05  W46-NAME    PIC X(40).
               05  W46-ADD1    PIC X(30).
               05  W46-ADD2    PIC X(30).
               05  W46-ADD3    PIC X(30).
               05  W46-ADD4    PIC X(30).
               05  W46-POST.
                   07  W46-PC  PIC 9(04).

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       01  W60-NME-ADD.
           03  W60-NAME.
               05  W60-NCHAR OCCURS 40 PIC X(01).
           03  W60-ADDRESS.
               05  W60-ACHAR OCCURS 60 PIC X(01).

       01  W80-ACCOUNT.
          03  W80-ACNO        PIC X(05).
           03  W80-NUMD REDEFINES W80-ACNO.
              05  W80-SERIAL  PIC 9(05).

       01  W95-STM.
           03  W95-COMP        PIC X(40).

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

       01  S03.
          03  LINE  4 COLUMN 12 FOREGROUND-COLOR Grey HIGHLIGHT
           VALUE "Printing Supplier labels".
          03  LINE  6 COLUMN 12 VALUE "Are you sure (".
    03        COLUMN 26 FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "Y".
    03        COLUMN 27 VALUE "/".
    03        COLUMN 28 FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "N".
    03        COLUMN 29 VALUE ")".

       01  S05.
          03  LINE 10 COLUMN 12 VALUE "Keyin Category code".

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
       AA00-MAIN       SECTION.
       AA00-INIT.
          IF LS0-CRLEV < 3
      *
      *      Insufficient security level
      *
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
               GO TO AA49.
             PERFORM ZA000-INIT.
      *
      *  ***  ****  ***** *   *   ****  ****  *** *   * ***** ***** ****
      * *   * *   * *     **  *   *   * *   *  *  **  *   *   *     *   *
      * *   * ****  ***   * * *   ****  ****   *  * * *   *   ***   ****
      * *   * *     *     *  **   *     *   *  *  *  **   *   *     *  *
      *  ***  *     ***** *   *   *     *   * *** *   *   *   ***** *   *
      *
      PERFORM OPEN-PRINTER.
      MOVE "P"   TO WS-COMMAND.
      PERFORM CH000.
      MOVE "C"   TO WS-COMMAND.
      PERFORM CALL-PRINTUTL.

       AA49.
            EXIT PROGRAM.

      *    *************************************************************
      *    ****    ROUTINES TO HANDLE VARIOUS FUNCTIONS FOR THE
      *           S C R E E N ,   K E Y B O A R D   &   M O U S E
      *    *************************************************************
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
      *    *************************************************************
      *            T H I S   R O U T I N E   I S   U S E D   T O
      *       D I S P L A Y   I N F O R M A T I O N   M E S S A G E S
      *    *************************************************************
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Move the message to WS-DSP-MES and the top line of the   ³
      *    ³  message box to WS-MES-LINE. If WS-MES-LINE is zero the   ³
      *    ³  default will be 20.                                      ³
      *    ³                  PERFORM DISPLAY-MESSAGE                  ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ****   T H I S   R O U T I N E   I S   U S E D   T O
      *            D I S P L A Y   E R R O R   M E S S A G E S
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

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³           ERASE SCREEN FROM SPECIFIED LOCATION            ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ Uses CRT-START as starting line and increases and CRT-END ³
      *    ³ as the ending line. The screen is cleared with Column 1   ³
      *    ³ and 80 containing "³" and columns 2 to 79 containing      ³
      *    ³ spaces.                                                   ³
      *    ³                                                           ³
      *    ³  eg.                                                      ³
      *    ³   MOVE 8         TO CRT-START.                            ³
      *    ³   MOVE 28        TO CRT-END.                              ³
      *    ³   PERFORM ERASE-SCREEN.                                   ³
      *    ³                                                           ³
      *    ³ To clear the entire screen and Display a new screen with  ³
      *    ³ headings (program/date/time/company):                     ³
      *    ³                                                           ³
      *    ³    CRT-PROGRAM (calling program)                          ³
      *    ³    CRT-HEADING (Screen heading)                           ³
      *    ³    CRT-COMPANY (Company name)                             ³
      *    ³    CRT-TERMINAL (consists of two fields:                  ³
      *    ³                  CRT-WRKHD and CRT-WRKST. See CRTHEAD)    ³
      *    ³    These fields should be populated at the start of the   ³
      *    ³    program.                                               ³
      *    ³    PERFORM HEADING-AND-CLEAR-SCREEN.                      ³
      *    ³                                                           ³
      *    ³ To change the screen heading:                             ³
      *    ³    MOVE "The new heading" TO CRT-HEADING.                 ³
      *    ³    PERFORM CHANGE-HEADING.                                ³
      *    ³                                                           ³
      *    ³ To change the time on the screen:                         ³
      *    ³    PERFORM CHANGE-TIME                                    ³
      *    ³                                                           ³
      *    ³ To clear lines 46 and 50:                                 ³
      *    ³    PERFORM CLEAR-L46-AND-50                               ³
      *    ³                                                           ³
      *    ³ To clear line 50:                                         ³
      *    ³    PERFORM CLEAR-L50                                      ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "CLEAR.CRT".

       COPY "ABORT.PRN".

      *
      *            ROUTINES TO HANDLE VARIOUS FUNCTIONS FOR THE
      *                PRINTER SUCH AS OPEN AND PRINT
      *
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³             PERFORM OPEN-PRINT-FILE                       ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *
      *         PRINT A LINE, ADVANCE LINE/PAGE, CHANGE STYLE ETC
      *     The following variables need to be set before calling this
      *     routine.
      *     
      *    COMMAND: C = Close printer
      *             E = End of report
      *             O = Open printer
      *             P = Print a line
      *             S = Skip to new page
      *
      *       WS-COMMAND      PIC  X(01).
      *
      *    PRINTER: D = Disk
      *             S = Screen
      *             1 = Lpt1
      *             2 = Lpt2
      *             3 = Lpt3
      *             4 = Com1
      *             5 = Com2
      *             8 = USB/WINDOWS
      *
      *       WS-PRINTER.
      *         WS-PRINT-NO PIC  9(01).
      *
      *    ADVANCE:   0 = Print - No advance
      *             1-3 = Print advance lines specified
      *             4-? = Print advance 1 line, then use a loop
      *                   to advance the remaining lines.
      *              99 = Print and advance to a new page.
      *
      *       WS-ADVANCE      PIC  9(02).
      *
      *    REPORT: 1 = Picking Slips
      *            2 = Invoices/Credit Notes
      *            3 = General Reports (Stock)
      *            4 = General Reports (Creditors)
      *            5 = General Reports (Debtors)
      *            6 = General Reports (G/Ledger)
      *            7 = Statements
      *            8 = Labels
      *            9 = Quotations
      *
      *      WS-REPORT           PIC  9(01).
      *
      *      See W02-PRN-TYPE for details of Print style.
      *
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³               PERFORM CALL-PRINTUTL                       ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´

       COPY "PRINTUTL.AP4".

      *
      *    ****    R E A D   F I L E S   R O U T I N E S
      *
       AC000-READ              SECTION.

       COPY "CONTROL.RD".

       COPY "PARAM.RD".

       COPY "CREDIT.RD".

      /
       AE155-GET-CREDITOR      SECTION.
       AE155-ACCOUNT.
            MOVE W80-ACNO  TO P-NUMBER.
             PERFORM READ-CREDIT THRU READ-CREDIT-EXIT.
           IF WS-F-ERROR = 3
               MOVE 1 TO WS-ERROR
           ELSE
          IF P-NUMBER NOT = W80-ACNO
               MOVE 2 TO WS-ERROR
           ELSE
               MOVE 0 TO WS-ERROR.
       AE160-EXIT.
             EXIT.

       CA175-ADDRESS           SECTION 2.
       CA175-EXTRACT.
             MOVE 0 TO WS-ERROR.
            UNSTRING P-ADDR DELIMITED BY "*"
              INTO W40-ADD1 W40-ADD2 W40-ADD3 W40-ADD4
               OVERFLOW
                 MOVE 1 TO WS-ERROR.
       CA180-EXIT.
             EXIT.
      /
      *    ****    S U P P L I E R   L A B E L S
      *
       CH000        SECTION 5.
       CH00.
             PERFORM ERASE-SCREEN.
             DISPLAY S03.
            ACCEPT WS-OPTION AT 0632
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
          IF WS-OPTION = "N"
               GO TO CH999.
            MOVE SPACES  TO R-DET.
            MOVE ALL "X"  TO W46-LABELS.
            MOVE "9999"  TO W46-PC (1) W46-PC (2).
       CH10.
           IF W46-ADD2 (1) = SPACES
               MOVE W46-POST (1) TO W46-ADD2 (1)
              MOVE SPACES  TO W46-POST (1)
               GO TO CH12.
           IF W46-ADD3 (1) = SPACES
               MOVE W46-POST (1) TO W46-ADD3 (1)
              MOVE SPACES  TO W46-POST (1)
               GO TO CH12.
           IF W46-ADD4 (1) = SPACES
               MOVE W46-POST (1) TO W46-ADD4 (1)
              MOVE SPACES  TO W46-POST (1).
       CH12.
           IF W46-ADD2 (2) = SPACES
               MOVE W46-POST (2) TO W46-ADD2 (2)
              MOVE SPACES  TO W46-POST (2)
               GO TO CH14.
           IF W46-ADD3 (2) = SPACES
               MOVE W46-POST (2) TO W46-ADD3 (2)
              MOVE SPACES  TO W46-POST (2)
               GO TO CH14.
           IF W46-ADD4 (2) = SPACES
               MOVE W46-POST (2) TO W46-ADD4 (2)
              MOVE SPACES  TO W46-POST (2).
       CH14.
            MOVE SPACES  TO LAB-L1.
            MOVE W46-NAME (1)  TO LAB-DET (1).
            MOVE W46-NAME (2)  TO LAB-DET (2).
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE W46-ADD1 (1)  TO LAB-DET (1).
            MOVE W46-ADD1 (2)  TO LAB-DET (2).
      PERFORM CALL-PRINTUTL.
            MOVE W46-ADD2 (1)  TO LAB-DET (1).
            MOVE W46-ADD2 (2)  TO LAB-DET (2).
      PERFORM CALL-PRINTUTL.
            MOVE W46-ADD3 (1)  TO LAB-DET (1).
            MOVE W46-ADD3 (2)  TO LAB-DET (2).
      PERFORM CALL-PRINTUTL.
            MOVE W46-ADD4 (1)  TO LAB-DET (1).
            MOVE W46-ADD4 (2)  TO LAB-DET (2).
      PERFORM CALL-PRINTUTL.
            MOVE W46-POST (1)  TO LAB-DET (1).
            MOVE W46-POST (2)  TO LAB-DET (2).
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO LAB-L1.
      MOVE 3   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE 1   TO WS-S6.
       CH15.
            PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO CH14.
       CH17.
             DISPLAY "A" AT 1212 WITH FOREGROUND-COLOR Cyan HIGHLIGHT
                     "ccount or " WITH FOREGROUND-COLOR Cyan
                     "N" WITH FOREGROUND-COLOR Cyan HIGHLIGHT
                     "ame sequence" WITH FOREGROUND-COLOR Cyan.
             MOVE "N"            TO WS-SEQ.
       CH17A.
      ACCEPT WS-SEQ AT 1237
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta UPDATE AUTO.
      CALL "CBL_TOUPPER" USING WS-SEQ
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
    IF NOT(WS-SEQ = "A" OR "N")
               GO TO CH17A.
      DISPLAY "Is this a restart " AT 1412
       "Y" WITH FOREGROUND-COLOR Brown HIGHLIGHT "/"
       "N" WITH FOREGROUND-COLOR Brown HIGHLIGHT.
      MOVE "N"   TO WS-OPTION.
       CH18.
      ACCEPT WS-OPTION AT 1434
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
    IF NOT(WS-OPTION = "N" OR "Y")
               GO TO CH18.
           IF WS-OPTION = "Y"
               GO TO CH40.
      MOVE " !"   TO P-NAME P-NUMBER.
       CH19.
      DISPLAY "Creditor account:" AT 1612.
           IF WS-SEQ = "A"
        PERFORM START-AT-CRED-AC THRU READ-CREDIT-EXIT
           ELSE
        PERFORM START-AT-CRED-NAME THRU READ-CREDIT-EXIT.
          IF WS-F-ERROR = 3
               GO TO CH999.
       CH20.
            PERFORM READ-CREDIT-NEXT THRU READ-CREDIT-EXIT.
          IF WS-F-ERROR = 3
               GO TO CH35.
       CH21.
      DISPLAY P-NUMBER AT 1630
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
            MOVE P-NAME  TO W40-NAME.
       CH34.
             PERFORM CA175-ADDRESS.
            MOVE W40-NAME  TO W46-NAME (WS-S6).
            MOVE W40-ADD1  TO W46-ADD1 (WS-S6).
            MOVE W40-ADD2  TO W46-ADD2 (WS-S6).
            MOVE W40-ADD3  TO W46-ADD3 (WS-S6).
            MOVE W40-ADD4  TO W46-ADD4 (WS-S6).
            MOVE P-PC   TO W46-PC (WS-S6).
            ADD 1   TO WS-S6.
           IF WS-S6 > 2
               PERFORM CH10 THRU CH14
              MOVE SPACES  TO W46-LABELS.
             PERFORM PRINT-ABORT.
           IF WS-OPTION = "Y"
               MOVE "**** PRINT ABORTED BY OPERATOR ****"
     TO R-DET
              GO TO CH50.
             GO TO CH20.
       CH35.
           IF WS-S6 > 1
               PERFORM CH10 THRU CH14.
             GO TO CH999.
       CH40.
             MOVE SPACES         TO W80-ACNO.
      MOVE 1801   TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 24.
            DISPLAY "Account No:" AT 1812.
            ACCEPT W80-ACNO AT 1824
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta AUTO.
    IF W80-ACNO = SPACES
               GO TO CH18.
      MOVE 5   TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING W80-ACNO
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
      MOVE 1   TO WS-LENGTH.
      MOVE W80-ACNO  TO P-NUMBER.
           IF WS-SEQ = "A"
               GO TO CH19.
      PERFORM READ-CREDIT THRU READ-CREDIT-EXIT.
      PERFORM START-AT-CRED-NAME THRU READ-CREDIT-EXIT.
       CH45.
      PERFORM READ-CREDIT-NEXT THRU READ-CREDIT-EXIT.
    IF W80-ACNO NOT = P-NUMBER
               GO TO CH45.
      DISPLAY "Creditor account:" AT 1612.
             GO TO CH21.

       CH50.
      MOVE 99   TO WS-ADVANCE.
      MOVE "E"   TO WS-COMMAND.
      PERFORM CALL-PRINTUTL.
      MOVE "P"   TO WS-COMMAND.

       CH999.
             EXIT.

      *    *************************************************************
      *                 I N I T I A L I S E   P R O G R A M
      *    *************************************************************
       ZA000-INIT       SECTION 9.
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

       ZA25.
             MOVE 1                  TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
             GO TO ZA999-EXIT.

      *COPY "ZA49.PRO".
      *
      *ZA200.
      *
      *COPY "LOCKED.PRO".
      *
      *ZA205.
      *      EXIT PROGRAM.

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
               WHEN 2          MOVE W02-NETWORK TO WS-FILE
                               MOVE WS-NETKEY   TO WS-KEY
               WHEN 3          MOVE W02-CREDIT  TO WS-FILE
                               MOVE P-NUMBER    TO WS-KEYX
               WHEN 15         MOVE WS-PARID    TO WS-FILE
                               MOVE WS-PARKEY   TO WS-KEY
               WHEN OTHER      MOVE "ERROR"     TO WS-FILE
             END-EVALUATE.

       COPY "DISPERR.PRO".
