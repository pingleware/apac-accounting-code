      $SET LINKCOUNT"512" GNT"DTP027.GNT"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******     ****     ******   ********   *
      *   **    **     **     **    **   **  **   **    **       **    *
      *   **    **     **     **    **  **    **       **       **     *
      *   **    **     **     *******   **    **     **        **      *
      *   **    **     **     **        **    **   **         **       *
      *   **    **     **     **         **  **   **          **       *
      *   *******      **     **          ****    ********    **       *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     D E B T O R   A L P H A B E T I C   L I S T   P R O G      *
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
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     DTP027.
       AUTHOR.         J W LEMMON (APAC).
       DATE-WRITTEN.   May 1982.

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

       COPY "PARAM.SL".

       COPY "DEBTOR.SL".

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
       77  WS-PARKEY               PIC  9(04)     COMP-5.
       77  WS-DEPKEY               PIC  9(04)     COMP-5.
       77  WS-DEBKEY               PIC  9(06)     COMP-5.
       77  WS-NETKEY               PIC  9(06)     COMP-5.
       77  WS-WRKEY                PIC  9(04)     COMP-5.
       77  WS-GST-RATE             PIC S9(03)V99  COMP-3.
       77  WS-NTP                  PIC  9(01).
       77  WS-LINES                PIC  9(02).
       77  WS-PAGE                 PIC  9(04)     COMP-5.
       77  WS-OPTION               PIC  X(01).
       77  WS-AGE                  PIC  X(01).
       77  WS-SEQ                  PIC  X(01).
       77  WS-CREDIT               PIC  X(01).
       77  WS-ZERO                 PIC  X(01).
       77  WS-NUM                  PIC  Z9.
       77  WS-ADDR                 PIC  X(01).
       77  WS-ERROR                PIC  9(01).
       77  WS-ERROR1               PIC  9(01).
       77  WS-PGE-LENGTH           PIC  9(02)     VALUE 66.
       77  WS-PRN-LENGTH           PIC  9(02)     VALUE 62.
       77  WS-EXPAND               PIC  X(01)     VALUE " ".
       77  WS-CHAR1                PIC  X(01).
       77  WS-CHAR2                PIC  X(01).
       77  WS-COMP                 PIC  9(01).
       77  WS-STM                  PIC  9(01).
       77  WS-TYPE                 PIC  X(01).
       77  WS-HEAD                 PIC  X(01).
       77  WS-BATCH                PIC  9(05).
       77  WS-MESS                 PIC  X(02).
       77  WS-ACNO                 PIC  X(06).
       77  WS-S1                   PIC  9(04)     COMP-5.
       77  WS-S2                   PIC  9(04)     COMP-5.
       77  WS-S3                   PIC  9(04)     COMP-5.
       77  WS-S4                   PIC  9(04)     COMP-5.
       77  WS-S5                   PIC  9(04)     COMP-5.
       77  WS-S6                   PIC  9(04)     COMP-5.
       77  WS-S7                   PIC  9(04)     COMP-5.
       77  WS-S8                   PIC  9(04)     COMP-5.
       77  TODAY-DDMMYY            PIC  9(08)     COMP-5.
       77  WS-USUB                 PIC  9(04)     COMP-5.
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

       01  W02-PRINTER-DETAILS.
           03  W02-PRINTER           PIC  X(12).
           03  W02-PGE-LENGTH        PIC  9(03).
           03  W02-PRN-LENGTH        PIC  9(03).
           03  W02-LINAGE            PIC  9(03).
           03  W02-PRN-STATUS        PIC  X(01)    VALUE "C".
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

           03  REP-LINE1                          REDEFINES W02-PRN-LINE.
               05  REP-DETAIL1     PIC  X(136).

           03  TRL-LINE1                          REDEFINES W02-PRN-LINE.
               05  TRL-DET         PIC  X(132).

           03  TRL-LINE6                          REDEFINES W02-PRN-LINE.
               05  TRL-PAGE        PIC  X(06).
               05  TRL-P-NO        PIC  Z(03)9.
               05  FILLER          PIC  X(24).
               05  TRL-RESTART.
                   07  TRL-COMPANY PIC  X(40).
                   07  TRL-HEAD    PIC  X(24).
               05  FILLER          PIC  X(18).
               05  TRL-DATE        PIC  X(06).
               05  TRL-DTE         PIC  Z9/99/9999.

           03  TRL-LINE6A                         REDEFINES W02-PRN-LINE.
               05  TRL-EXPP        PIC  X(06).
               05  FILLER          PIC  X(05).
               05  TRL-CO          PIC  X(30).
               05  TRL-EXHEAD      PIC  X(24).
               05  TRL-EXPC        PIC  X(07).
           
           03  ALF-LINE1                          REDEFINES W02-PRN-LINE.
               05  ALF-HEAD1       PIC  X(07).
               05  ALF-HEAD2       PIC  X(42).
               05  ALF-HEAD3       PIC  X(62).
               05  ALF-HEAD4       PIC  X(06).
               05  ALF-HEAD5       PIC  X(15).

           03  ALF-LINE2                          REDEFINES W02-PRN-LINE.
               05  ALF-AC          PIC  X(06).
               05  FILLER          PIC  X(01).
               05  ALF-NME         PIC  X(40).
               05  FILLER          PIC  X(02).
               05  ALF-ADDR        PIC  X(60).
               05  FILLER          PIC  X(02).
               05  ALF-PCDE        PIC  9(04).
               05  FILLER          PIC  X(02).
               05  ALF-ID          PIC  X(14).
               05  FILLER          PIC  X(01).

       01  W05-LIST.
           03  W05-OPT         PIC 9(03).
           03  W05-TYPES REDEFINES W05-OPT.
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

       01  W24-MONTHS.
           03  W24-MONTH-NAMES.
               05  FILLER      PIC  X(09) VALUE " January ".
               05  FILLER      PIC  X(09) VALUE " February".
               05  FILLER      PIC  X(09) VALUE "  March  ".
               05  FILLER      PIC  X(09) VALUE "  April  ".
               05  FILLER      PIC  X(09) VALUE "   May   ".
               05  FILLER      PIC  X(09) VALUE "   June  ".
               05  FILLER      PIC  X(09) VALUE "   July  ".
               05  FILLER      PIC  X(09) VALUE "  August ".
               05  FILLER      PIC  X(09) VALUE "September".
               05  FILLER      PIC  X(09) VALUE " October ".
               05  FILLER      PIC  X(09) VALUE " November".
               05  FILLER      PIC  X(09) VALUE " December".
           03  W24-EACH-MONTH REDEFINES W24-MONTH-NAMES.
               05  W24-MONTH   PIC  X(09) OCCURS 12.

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

       COPY "W40.DBT".

       COPY "W40.WS".

       01  W45-TRAN.
           03  W45-CODE        PIC 9(02).
           03  W45-ENG         PIC X(12).
           03  W45-AFR         PIC X(12).
           03  W45-ACT         PIC X(01).
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

       COPY "W75.WS".

       01  W80-ACCOUNT.
           03  W80-ACNO        PIC X(06).
           03  W80-NUMD REDEFINES W80-ACNO.
               05  W80-SERIAL  PIC 9(06).
           03  W80-TYPE        PIC 9(01).
       01  W85-BATCHES.
               05  W85-BATCH   PIC 9(05).

       01  W95-STM.
           03  W95-COMP        PIC X(40).

       01  W100-HEAD.
           03  FILLER          PIC X(26)    VALUE
                             "DEBTOR ACCOUNTS LISTING -".
           03  W100-DESC       PIC X(11).

       01  W100-EDIT.
           03  W100-LBAL       PIC Z(06)9.99-.
           03  W100-LARR       PIC Z(06)9.99-.
           03  W100-BAL        PIC Z(08)9.99-.
           03  W100-CUR        PIC Z(08)9.99-.
           03  W100-30         PIC Z(08)9.99-.
           03  W100-60         PIC Z(08)9.99-.
           03  W100-90         PIC Z(08)9.99-.
           03  W100-120        PIC Z(08)9.99-.
           03  W100-INT        PIC Z(08)9.99.
           03  W100-CRL        PIC Z(06)9.
           03  W100-KEY        PIC Z(05)9.

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
           IF LS0-DBLEV < 2
      *
      *      Insufficient security level
      *
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
               GO TO AA49.
            PERFORM ZA00-INIT.
      PERFORM XD000.

       AA49.
            EXIT PROGRAM.

      * COPY "DATE.CHK".

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

      /
       CA155-GET-DEBTOR        SECTION 2.
       CA155-ACCOUNT.
             MOVE W80-ACNO       TO DEB-ACNO.
             PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 6
               MOVE 1            TO WS-ERROR
           ELSE
           IF DEB-ACNO NOT = W80-ACNO
               MOVE 2            TO WS-ERROR
           ELSE
               MOVE 0            TO WS-ERROR
               MOVE DEB-TYPE     TO W80-TYPE.

       CA160-EXIT.
             EXIT.

       CA165-NAME              SECTION 2.
       CA165-EXTRACT.
             MOVE ZERO TO WS-ERROR.
             UNSTRING W60-NAME DELIMITED BY "*" OR "!"
               INTO W40-SURNAME W40-FNAME W40-SNAME
               OVERFLOW
                 MOVE 1 TO WS-ERROR.

       CA170-EXIT.
             EXIT.

       CA175-ADDRESS           SECTION 2.
       CA175-EXTRACT.
             MOVE 0 TO WS-ERROR.
             UNSTRING W60-ADDRESS DELIMITED BY "*"
           INTO W40-ADD1 W40-ADD2
          W40-ADD3 W40-ADD4
               OVERFLOW
         MOVE 1   TO WS-ERROR.

       CA180-EXIT.
             EXIT.
      /
       XD000        SECTION 5.
       XD00-INIT.
             PERFORM ERASE-SCREEN.
             DISPLAY "ALPHA LIST" AT 0334 WITH FOREGROUND-COLOR Grey HIGHLIGHT .
             DISPLAY "Printing for" AT 0512.

       XD05-ACCEPT.
            DISPLAY W95-COMP AT 0527
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
             MOVE "Are you sure Y/N" TO WS-OPT-MES.
             MOVE 1                  TO WS-INSTR.
             PERFORM OPT-MESSAGE.
          IF WS-OPTION = "N"
               GO TO XD999-EXIT.

       XD08.
             MOVE 1              TO WS-SUB-2.
             MOVE 0              TO WS-PAGE.
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
    IF NOT (WS-PRINTER = "S")
        MOVE 0   TO WS-ADVANCE
        MOVE 3   TO W02-PRN-TYPE
        PERFORM CALL-PRINTUTL.
      PERFORM RESTORE-SCREEN-3.

       XD10-HEADING.
      MOVE "H"   TO W02-PRN-TYPE.
            MOVE SPACES  TO REP-DETAIL1.
            MOVE "PAGE:"  TO TRL-PAGE.
            MOVE "DATE:"  TO TRL-DATE.
            ADD 1   TO WS-PAGE.
            MOVE W12-TODAY  TO TRL-DTE.
            MOVE WS-PAGE  TO TRL-P-NO.
      *
      *    ****   T E S T   I F   O U T P U T T O   P R I N T E R
      *    O R S P O O L  F I L E
      *
    IF NOT (WS-PRINTER = "S")
        MOVE 0   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
        MOVE SPACES  TO TRL-LINE1
        MOVE "E"   TO W02-PRN-TYPE
        PERFORM CALL-PRINTUTL
        MOVE SPACES  TO TRL-LINE1.
            MOVE W95-COMP TO TRL-CO.
             MOVE " - ALPHA LIST" TO TRL-EXHEAD.
      MOVE "H"   TO W02-PRN-TYPE.
      *
      *    ****   T E S T   I F   O U T P U T T O   P R I N T E R
      *    O R S P O O L  F I L E
      *
    IF NOT (WS-PRINTER = "S")
        MOVE 0   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      MOVE SPACES  TO TRL-LINE1.
    IF NOT (WS-PRINTER = "S")
        MOVE 0   TO WS-ADVANCE
        MOVE "X"   TO W02-PRN-TYPE
        PERFORM CALL-PRINTUTL
              MOVE 3   TO W02-PRN-TYPE
        PERFORM CALL-PRINTUTL.
      MOVE "H"   TO W02-PRN-TYPE.
            MOVE SPACES TO TRL-DET.

       XD12-HEADING.
             MOVE "AC-NO"        TO ALF-HEAD1.
             MOVE "NAME"         TO ALF-HEAD2.
             MOVE "ADDRESS"      TO ALF-HEAD3.
             MOVE "CODE"         TO ALF-HEAD4.
             MOVE "TAX/ID NO.  " TO ALF-HEAD5.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO TRL-DET.
      MOVE "D"   TO W02-PRN-TYPE.

       XD13.
      PERFORM RESTORE-SCREEN-3.
            DISPLAY "Is this a restart (" AT 1612
       "Y" WITH FOREGROUND-COLOR Brown HIGHLIGHT "/"
       "N" WITH FOREGROUND-COLOR Brown HIGHLIGHT ")".

       XD14.
            ACCEPT WS-OPTION AT 1636
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
          IF NOT (WS-OPTION = "N" OR "Y")
               GO TO XD14.
             MOVE SPACES         TO WS-ACNO.
           IF WS-OPTION = "N"
               GO TO XD15-INDEX.

       XD14A.
            DISPLAY WS-BLNK78 AT 1802 WITH FOREGROUND-COLOR Cyan.
             DISPLAY "Account No:" AT 1812.
            ACCEPT W80-ACNO AT 1824
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta AUTO.
           IF W80-ACNO = SPACES
               GO TO XD15-INDEX.
             PERFORM CA155-GET-DEBTOR.
            DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR Cyan.
           IF WS-ERROR NOT = ZERO
              MOVE "Invalid account number"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
               GO TO XD14A.
            MOVE DEB-NAME  TO W40-NAME.
           IF DEB-TYPE = 1
               INSPECT W40-NAME REPLACING ALL "*" BY "."
               INSPECT W40-NAME REPLACING FIRST "!" BY " ".
            DISPLAY W40-NAME AT 1831
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO XD14A.
            MOVE W80-ACNO  TO WS-ACNO.
             MOVE "RESTART  RESTART  RESTART  RESTART RESTART RESTART"
           TO TRL-RESTART.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO TRL-DET.

       XD15-INDEX.
             DISPLAY "Debtor account:" AT 2212.
             MOVE " !"           TO DEB-NAME.
             PERFORM START-AT-DEBT-NAME THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 6
              GO TO XD990-CLOSE.

       XD20.
             PERFORM PRINT-ABORT.
    IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
              MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO REP-DETAIL1
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              GO TO XD990-CLOSE.
             PERFORM READ-DEBTOR-NEXT THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 6
               GO TO XD25.
      *
      *    ****   T E S T   I F   O U T P U T T O   P R I N T E R
      *    O R S P O O L  F I L E
      *
    IF NOT (WS-PRINTER = "S")
        DISPLAY DEB-ACNO AT 2228
         WITH FOREGROUND-COLOR Cyan HIGHLIGHT
       BACKGROUND-COLOR Magenta.
           IF WS-ACNO = DEB-ACNO
               MOVE SPACES       TO WS-ACNO.
             MOVE DEB-NAME       TO W40-NAME.
           IF DEB-TYPE = 1
               INSPECT W40-NAME REPLACING ALL "*" BY "."
               INSPECT W40-NAME REPLACING FIRST "!" BY " ".
            MOVE DEB-ACNO  TO ALF-AC.
            MOVE W40-NAME  TO ALF-NME.
           IF WS-ACNO NOT = SPACES
               GO TO XD20.
             MOVE DEB-ADDRESS    TO W60-ADDRESS.
             INSPECT W60-ADDRESS REPLACING ALL "*" BY "."
             MOVE W60-ADDRESS    TO ALF-ADDR.
            MOVE DEB-POST  TO ALF-PCDE.
             MOVE DEB-ID         TO ALF-ID.
    IF W02-LINAGE < W02-PRN-LENGTH
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              PERFORM XD10-HEADING THRU XD12-HEADING.
             MOVE SPACES TO TRL-DET.
             GO TO XD20.

       XD25.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE "****  E N D   O F   L I S T  ****"
     TO ALF-NME.
      MOVE 99   TO WS-ADVANCE.
      MOVE "E"   TO WS-COMMAND.
      PERFORM CALL-PRINTUTL.
      MOVE "P"   TO WS-COMMAND.

       XD990-CLOSE.
      MOVE "C"   TO WS-COMMAND.
      PERFORM CALL-PRINTUTL.

       XD999-EXIT.
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
      *    ³ D o   n o t   d i s p l a y   E R R O R   m e s s a g e s ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      *      MOVE 44                 TO USER-NUMBER.
      *      MOVE 1                  TO USER-SETTING.
      *      CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

             COPY "FUNCTION.PRO".

       ZA25.
             MOVE 4                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-AGE            TO WS-AGE.
             MOVE 1                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-COMPANY        TO W95-COMP.
             MOVE PAR-DMY            TO W12-TODAY.
             MOVE PAR-YMD            TO W12-T-YMD.
           IF PAR-STMINV = "S" OR "X"
               MOVE "N"              TO WS-HEAD
           ELSE
               MOVE "Y"              TO WS-HEAD.
             MOVE 1                  TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
             GO TO ZA999-EXIT.

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
               WHEN OTHER      MOVE "ERROR"       TO WS-FILE
                               MOVE "DTP027"      TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
