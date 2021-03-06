      $SET LINKCOUNT"384" GNT"DTP019.GNT"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******     ****       **     ******     *
      *   **    **     **     **    **   **  **     ***    **    **    *
      *   **    **     **     **    **  **    **     **    **    **    *
      *   **    **     **     *******   **    **     **     *******    *
      *   **    **     **     **        **    **     **          **    *
      *   **    **     **     **         **  **      **    **    **    *
      *   *******      **     **          ****     ******   ******     *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     D E B T O R   R E C E I P T I N G   R E P O R T S          *
      *                                                                *
      *       Version 9.04.01 - May 2016                               *
      *                                                                *
      ******************************************************************
      *                                                                *
      *  May 2006 - New field included in Accounts Receivable          *
      *             for recording number of outstanding Jobs           *
      *             linked to the account. Additional filler           *
      *             added for future expansion.                        *
      *                                                                *
      *  Nov 2012 - Changed DEB-ADDRESS and DEB-PADDR from group       *
      *             fields with 70 characters to group fields          *
      *             containing DEB-ADD-L1/2/3/4 and DEB-PADD-L1/2/3/4  *
      *             respectively. Each of these new fields are defined *
      *             as PIC X(30). As new PC's and Servers contain huge *
      *             amounts of disk space this has been changed, which *
      *             also results in simplifying the code for handling  *
      *             addresses in Data capture, enquiries, reports etc. *
      *             This also removes the restriction on the number of *
      *             characters that an address may contain.            *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     DTP019.
       AUTHOR.         J W LEMMON (APAC).
       DATE-WRITTEN.   June 2002.

                COPYRIGHT NOTICE: COPYRIGHT (C) 2002 - 2017
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

       COPY "RECEIPTS.SL".

       DATA DIVISION.
      *
      *    ********  ********  **        ******** 
      *    **           **     **        **
      *    **           **     **        ** 
      *    *****        **     **        *****
      *    **           **     **        **  
      *    **           **     **        **
      *    **        ********  ********  ********
      *
       FILE SECTION.

       COPY "CONTROL.FDE".

       COPY "PARAM.FDE".

       COPY "RECEIPTS.FDE".

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
       77  WS-MSUB                 PIC  9(04)    COMP-5.
       77  WS-PARKEY               PIC  9(04)    COMP-5.
       77  WS-NETKEY               PIC  9(06)    COMP-5.
       77  WS-WRKEY                PIC  9(04)    COMP-5.
       77  WS-NTP                  PIC  9(01).
       77  WS-LINES                PIC  9(02).
       77  WS-PAGE                 PIC  9(04)    COMP-5.
       77  WS-OPTION               PIC  X(01).
       77  WS-AGE                  PIC  X(01).
       77  WS-SEQ                  PIC  X(01).
       77  WS-CREDIT               PIC  X(01).
       77  WS-ZERO                 PIC  X(01).
       77  WS-USER                 PIC  X(03).
       77  WS-ERROR                PIC  9(01).
       77  WS-ERROR1               PIC  9(01).
       77  WS-PGE-LENGTH           PIC  9(02)           VALUE 66.
       77  WS-PRN-LENGTH           PIC  9(02)           VALUE 62.
       77  WS-EXPAND               PIC  X(01)           VALUE " ".
       77  WS-PRN-NO               PIC  X(01)    COMP-X VALUE 9.
       77  WS-PRN-STAT             PIC  X(01)    COMP-X.
       77  WS-COMP                 PIC  9(01).
       77  WS-STM                  PIC  9(01).
       77  WS-BDT                  PIC  9(01).
       77  WS-BAD                  PIC  X(01).
       77  WS-TYPE                 PIC  X(01).
       77  WS-MESS                 PIC  X(02).
       77  WS-ACNO                 PIC  X(06)           VALUE SPACES.
       77  WS-S1                   PIC  9(04)    COMP-5.
       77  WS-S2                   PIC  9(04)    COMP-5.
       77  WS-S3                   PIC  9(04)    COMP-5.
       77  WS-S4                   PIC  9(04)    COMP-5.
       77  WS-S5                   PIC  9(04)    COMP-5.
       77  WS-S6                   PIC  9(04)    COMP-5.
       77  WS-S7                   PIC  9(04)    COMP-5.
       77  WS-S8                   PIC  9(04)    COMP-5.
       77  WS-IXS1                 PIC  9(04)    COMP-5.
       77  PRG-NAME                PIC  X(12)           VALUE SPACES.
       77  PRG-DEBT-LUP            PIC  X(12)           VALUE "DTP\DTPLOOK".
       77  TODAY-DDMMYY            PIC  9(08)    COMP-5.
       77  WS-USUB                 PIC  9(04)    COMP-5.
      *
      *    旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커
      *    � Used by calling program (WORKING-STORAGE) and called program �
      *    � (LINKAGE SECTION) for Main screen layout and headings.       �
      *    읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴켸
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
           03  CRT-START           PIC  9(02) VALUE 5.
           03  CRT-END             PIC  9(02) VALUE 46.
      *
      *    Calling Program
      *
           03  CRT-PROGRAM         PIC  X(15) VALUE "DTP019".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "ACCOUNTS RECEIVABLE - RECEIPTING REPORTS".
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
           03  WS-SYS-ID           PIC  X(03).

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

           03  R-SL1                             REDEFINES W02-PRN-LINE.
               05  R-SLA           PIC  X(78).
               05  FILLER          PIC  X(58).
           03  R-SL2                             REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(58).
               05  R-SLB           PIC  X(78).

          03  R-L1                               REDEFINES W02-PRN-LINE.
              05  R-DET1           PIC  X(136).

          03  R-L2                               REDEFINES W02-PRN-LINE.
              05  R-DET2           PIC  X(80).

          03  R-L3                               REDEFINES W02-PRN-LINE.
              05  REC-PAGE         PIC  X(06).
              05  REC-P-NO         PIC  Z(03)9.
              05  FILLER           PIC  X(24).
              05  REC-COMPANY      PIC  X(40).
              05  REC-HEAD         PIC  X(24).
              05  FILLER           PIC  X(17).
              05  REC-DATE         PIC  X(06).
              05  REC-DTE          PIC  Z9/99/9999.

          03  R-L5                               REDEFINES W02-PRN-LINE.
              05  FILLER           PIC  X(01).
              05  REC-AC           PIC  X(06).
              05  FILLER           PIC  X(02).
              05  REC-NAME         PIC  X(40).
              05  FILLER           PIC  X(02).
              05  REC-DTE2         PIC  Z9/99/9999.
              05  FILLER           PIC  X(02).
              05  REC-REF          PIC  X(08).
              05  FILLER           PIC  X(02).
              05  REC-VAL          PIC  Z(08)9.99- BLANK WHEN ZERO.
              05  FILLER           PIC  X(02).
              05  REC-CSH          PIC  Z(08)9.99- BLANK WHEN ZERO.
              05  FILLER           PIC  X(02).
              05  REC-CHQ          PIC  Z(08)9.99- BLANK WHEN ZERO.
              05  FILLER           PIC  X(02).
              05  REC-TRF          PIC  Z(08)9.99- BLANK WHEN ZERO.
              05  FILLER           PIC  X(01).

          03  R-L6                               REDEFINES W02-PRN-LINE.
              05  REC-DET          PIC  X(132).

          03  R-L7                               REDEFINES W02-PRN-LINE.
              05  FILLER           PIC  X(01).
              05  REC-HEAD1        PIC  X(08).
              05  REC-HEAD2        PIC  X(45).
              05  REC-HEAD3        PIC  X(09).
              05  REC-HEAD4        PIC  X(10).
              05  REC-HEAD5        PIC  X(17).
              05  REC-HEAD6        PIC  X(13).
              05  REC-HEAD7        PIC  X(15).
              05  REC-HEAD8        PIC  X(14).

       01  W05-LIST.
           03  W05-OPT             PIC 9(03).
           03  W05-TYPES                         REDEFINES W05-OPT.
               05  W05-COMP        PIC 9(01).
               05  W05-INDV        PIC 9(01).
               05  W05-INT         PIC 9(01).

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

       01  W25-CALCS.
           03  W25-RESULT          PIC  9(05)V99.
           03  W25-RESULT1                       REDEFINES W25-RESULT.
               05  W25-WHOLE       PIC  9(05).
               05  W25-DECIMAL     PIC  9(02).
           03  W25-RESULT2                       REDEFINES W25-RESULT.
               05  W25-KEY         PIC  9(04).
               05  W25-SUB         PIC  9(01).
               05  FILLER          PIC  9(02).
           03  W25-TOTAL           PIC S9(07)V99 COMP-3.
           03  W25-VALUE           PIC S9(07)V99 COMP-3.

       COPY "W40.DBT".

       COPY "W40.WS".

       COPY "FUNCTION.WS".

      *COPY "W50.WS".

       01  W75-KEYS.
           03  W75-S          PIC 9(02) COMP-5.
           03  W75-S1         PIC 9(02) COMP-5.
           03  W75-DNO        PIC X(06) OCCURS 18.

       01  W80-ACCOUNT.
           03  W80-ACNO        PIC X(06).
           03  W80-NUMD REDEFINES W80-ACNO.
               05  W80-SERIAL  PIC 9(06).
      *    03  W80-TYPE        PIC 9(01).

       01  W90-TRAN.
          03  W90-DATE        PIC X(10).
           03  W90-DTE REDEFINES W90-DATE
                PIC 99/99/9999.
          03  W90-REF        PIC X(08).
          03  W90-VAL        PIC S9(09)V99 COMP-3.
          03  W90-CSH        PIC S9(09)V99 COMP-3.
          03  W90-CHQ        PIC S9(09)V99 COMP-3.
          03  W90-TRF        PIC S9(09)V99 COMP-3.

       01  W95-STM.
           03  W95-COMP        PIC X(40).

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
      *     *****    *****   ******   ******  ******  **    **
      *    **   **  **   **  **   **  **      **      ***   **
      *    **       **       **  **   **      **      ****  **
      *     *****   **       *****    *****   *****   ** ** **
      *         **  **       **  **   **      **      **  ****
      *    **   **  **   **  **   **  **      **      **   ***
      *     *****    *****   **   **  ******  ******  **    **
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
       AA00-MAIN       SECTION.
       AA00-INIT.
           IF LS0-DBLEV < 2
      *
      *      Insufficient security level
      *
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
               GO TO AA49.
             PERFORM ZA000-INIT.
             PERFORM DA000.

       AA49.
            EXIT PROGRAM.

       COPY "AA900.ALM".
       
       COPY "DATE.CNV".

      *    *************************************************************
      *    ****    ROUTINES TO HANDLE VARIOUS FUNCTIONS FOR THE
      *           S C R E E N ,   K E Y B O A R D   &   M O U S E
      *    *************************************************************
      *    旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
      *    �      SAVE-SCREEN /-2/-3  and  RESTORE-SCREEN /-2/-3       �
      *    팠袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴�
      *    �                      SCREEN-SHADOW                        �
      *    쳐컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
      *    � To routine is used to display a shadow down the right and �
      *    � along the bottom of a pop-up box. The parameters that are �
      *    � required:                                                 �
      *    �          SHADE-ROW   - Top line of the box.               �
      *    �          SHADE-COL   - Left line of box.                  �
      *    �          SHADE-WIDTH - Width of the box.                  �
      *    �          SHADE-LINES - Height of box.                     �
      *    팠袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴�
      *    �                      CHECK-CORRECT                        �
      *    쳐컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
      *    � This routine displays a pop-up window with the message    �
      *    �           "Press Y if correct - N if incorrect"           �
      *    �                                                           �
      *    � The response is returned in WS-OPTION (in upper case).    �
      *    �                                                           �
      *    � To start with the pop-up window higher or lower than row  �
      *    � 20 (default); move another value to WS-MES-LINE.          �
      *    *************************************************************
      *            T H I S   R O U T I N E   I S   U S E D   T O
      *       D I S P L A Y   I N F O R M A T I O N   M E S S A G E S
      *    *************************************************************
      *    旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
      *    �  Move the message to WS-DSP-MES and the top line of the   �
      *    �  message box to WS-MES-LINE. If WS-MES-LINE is zero the   �
      *    �  default will be 20.                                      �
      *    �                  PERFORM DISPLAY-MESSAGE                  �
      *    쳐컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
      *    팠袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴�
      *    ****   T H I S   R O U T I N E   I S   U S E D   T O
      *            D I S P L A Y   E R R O R   M E S S A G E S
      *    *************************************************************
      *    旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
      *    �                      ERROR-MESSAGE                        �
      *    쳐컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
      *    � To display the error message higher or lower (default is  �
      *    � line 20) Move the line number which must be used as the   �
      *    � top line to WS-MES-LINE. The message to be displayed must �
      *    � be in WS-ERR-MES. PERFORM ERROR-MESSAGE.                  �
      *    읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�

       COPY "FUNCTION.CRT".

      *    旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
      *    �                        OPT-MESSAGE                        �
      *    쳐컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
      *    � This routine is used to allow the OPERATOR to respond to  �
      *    � a request for an option, so that the correct action can   �
      *    � be performed by the program. The routine will display the �
      *    � message in a pop-up window and allow the OPERATOR to      �
      *    � respond to the 'question'.                                �
      *    �                                                           �
      *    � The option request must be placed in WS-OPT-MES and may   �
      *    � not exceed 48 characters. The message will be centred in  �
      *    � the window. An example of a message request follows:      �
      *    �                                                           �
      *    �   MOVE "Print transactions (Y/N) [ ]" TO WS-OPT-MES.      �
      *    �   MOVE Instruction    TO WS-INSTR.                        �
      *    �       [see Accptopt for instruction values]               �
      *    �   PERFORM OPT-MESSAGE.                                    �
      *    �                                                           �
      *    � This would be displayed as:                               �
      *    �      旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커   �
      *    �      �          Print transactions (Y/N) [ ]          낡� �
      *    �      읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴켸같 �
      *    �        같같같같같같같같같같같같같같같같같같같같같같같같같 �
      *    �                                                           �
      *    � The response is returned in WS-OPTION (in upper case).    �
      *    �                                                           �
      *    � The system will display the message box with the top line �
      *    � as the value of WS-MES-LINE. If WS-MES-LINE is zero the   �
      *    � default will be 20.                                       �
      *    읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�

       COPY "OPTION.CRT".

       COPY "LOCKED.REC".

      *    旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
      *    �           ERASE SCREEN FROM SPECIFIED LOCATION            �
      *    쳐컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
      *    � Uses CRT-START as starting line and increases and CRT-END �
      *    � as the ending line. The screen is cleared with Column 1   �
      *    � and 80 containing "�" and columns 2 to 79 containing      �
      *    � spaces.                                                   �
      *    �                                                           �
      *    �  eg.                                                      �
      *    �   MOVE 8         TO CRT-START.                            �
      *    �   MOVE 28        TO CRT-END.                              �
      *    �   PERFORM ERASE-SCREEN.                                   �
      *    �                                                           �
      *    � To clear the entire screen and Display a new screen with  �
      *    � headings (program/date/time/company):                     �
      *    �                                                           �
      *    �    CRT-PROGRAM (calling program)                          �
      *    �    CRT-HEADING (Screen heading)                           �
      *    �    CRT-COMPANY (Company name)                             �
      *    �    CRT-TERMINAL (consists of two fields:                  �
      *    �                  CRT-WRKHD and CRT-WRKST. See CRTHEAD)    �
      *    �    These fields should be populated at the start of the   �
      *    �    program.                                               �
      *    �    PERFORM HEADING-AND-CLEAR-SCREEN.                      �
      *    �                                                           �
      *    � To change the screen heading:                             �
      *    �    MOVE "The new heading" TO CRT-HEADING.                 �
      *    �    PERFORM CHANGE-HEADING.                                �
      *    �                                                           �
      *    � To change the time on the screen:                         �
      *    �    PERFORM CHANGE-TIME                                    �
      *    �                                                           �
      *    � To clear line 50:                                         �
      *    �    PERFORM CLEAR-L50                                      �
      *    읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�

       COPY "CLEAR.CRT".

       COPY "ABORT.PRN".

       COPY "PRINTUTL.AP5".

      *
      *    ****    READ FILES ROUTINES
      *
       AC000-READ              SECTION.

       COPY "CONTROL.RD".

       COPY "PARAM.RD".

       COPY "RECEIPTS.RD".

      *
      *    ****    D E B T O R   R E C E I P T I N G   R E P O R T S
      *
       DA000        SECTION 5.

       DA00.
             PERFORM HEADING-AND-CLEAR-SCREEN.      
             MOVE LS-USER            TO WS-USER.
             MOVE ZERO               TO WS-BDT.
             MOVE "N"                TO WS-BAD.

       DA05.
             DISPLAY W95-COMP AT 0527 WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
             MOVE "Are you sure Y/N" TO WS-OPT-MES.
             MOVE 1                  TO WS-INSTR.
             PERFORM OPT-MESSAGE.
           IF WS-OPTION = "N"
               GO TO DA999-EXIT.
             DISPLAY "Print reports for Work Station " AT 0912 WS-USER WITH FOREGROUND-COLOR Brown HIGHLIGHT.
           IF NOT(LS-USER = LS-SYS-ID)
               GO TO DA15.

       DA10.
             ACCEPT WS-USER AT 0943 WITH FOREGROUND-COLOR Brown HIGHLIGHT UPDATE AUTO.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY   GO TO DA999-EXIT
                 WHEN OTHER     PERFORM AA900-ALARM
                                GO TO DA10
               END-EVALUATE.
           IF WS-USER = SPACES
               GO TO DA999-EXIT.
             MOVE 3                  TO WS-LENGTH
             CALL "CBL_TOUPPER" USING WS-USER BY VALUE WS-LENGTH RETURNING WS-STATUS.
             DISPLAY WS-USER AT 0943 WITH FOREGROUND-COLOR Grey HIGHLIGHT.
             MOVE 1                  TO WS-LENGTH.

       DA15.
             OPEN INPUT RECEIPTS.
           IF NOT(WS-STATUS = "00")
               MOVE "No details for Work Station " TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO DA10.
             MOVE "  !"              TO RCT-REF.
             PERFORM START-AT-RECT-REF THRU READ-RECEIPTS-EXIT.
           IF NOT(WS-STATUS = "00")
               GO TO DA999-EXIT.
             MOVE 0                  TO WS-PAGE W90-VAL W90-CSH W90-CHQ W90-TRF.
      *
      *  ***  ****  ***** *   *   ****  ****  *** *   * ***** ***** ****
      * *   * *   * *     **  *   *   * *   *  *  **  *   *   *     *   *
      * *   * ****  ***   * * *   ****  ****   *  * * *   *   ***   ****
      * *   * *     *     *  **   *     *   *  *  *  **   *   *     *  *
      *  ***  *     ***** *   *   *     *   * *** *   *   *   ***** *   *
      *
             PERFORM OPEN-PRINTER.
             MOVE "P"                TO WS-COMMAND.
           IF NOT(WS-PRINTER = "S")
               MOVE 0                TO WS-ADVANCE
               MOVE 3                TO W02-PRN-TYPE
               PERFORM CALL-PRINTUTL.

       DA20.
             MOVE "H"                TO W02-PRN-TYPE.
             MOVE SPACES             TO R-DET1.
             MOVE "PAGE:"            TO REC-PAGE.
             MOVE "DATE:"            TO REC-DATE.
             ADD 1                   TO WS-PAGE.
             MOVE W12-TODAY          TO REC-DTE.
             MOVE WS-PAGE            TO REC-P-NO.
             MOVE 0                  TO WS-ADVANCE.
      *
      *    ****   T E S T   I F   O U T P U T   T O   P R I N T E R
      *           O R   S P O O L   F I L E
      *
           IF NOT(WS-PRINTER = "S")
               PERFORM CALL-PRINTUTL
               MOVE SPACES           TO R-L1
               MOVE "E"              TO W02-PRN-TYPE
               PERFORM CALL-PRINTUTL.
             MOVE W95-COMP           TO REC-COMPANY.
             MOVE " - RECEIPTS LISTING" TO REC-HEAD.
             MOVE "H"                TO W02-PRN-TYPE.
      *
      *    ****   T E S T   I F   O U T P U T   T O   P R I N T E R
      *           O R   S P O O L   F I L E
      *
           IF NOT(WS-PRINTER = "S")
               MOVE 0                TO WS-ADVANCE
               PERFORM CALL-PRINTUTL.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO DETAR-L1.
           IF NOT(WS-PRINTER = "S")
               MOVE 0                TO WS-ADVANCE
               MOVE "X"              TO W02-PRN-TYPE
               PERFORM CALL-PRINTUTL
               MOVE 3                TO W02-PRN-TYPE
               PERFORM CALL-PRINTUTL.
             MOVE "H"                TO W02-PRN-TYPE.
             MOVE SPACES             TO R-DET1.
             MOVE 2                  TO WS-ADVANCE.

       DA25.
             MOVE "AC.NO."           TO REC-HEAD1.
             MOVE "NAME"             TO REC-HEAD2.
             MOVE "DATE"             TO REC-HEAD3.
             MOVE "REF. NO."         TO REC-HEAD4.
             MOVE "RECEIPT VAL."     TO REC-HEAD5
             MOVE "CASH VALUE"       TO REC-HEAD6
             MOVE "CHEQUE VALUE"     TO REC-HEAD7
             MOVE "TRANSFER VAL"     TO REC-HEAD8.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DET1.
             MOVE "D"                TO W02-PRN-TYPE.
      *
      *    ****   T E S T   I F   O U T P U T   T O   S C R E E N
      *
       DA30.
           IF WS-PRINTER = "S"
               GO TO DA40.

       DA35.
             DISPLAY "Reference Numb:" AT 1112.

       DA40.
             PERFORM PRINT-ABORT.
           IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
               MOVE "**** PRINT ABORTED BY OPERATOR ****" TO R-DET1
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               GO TO DA999-EXIT.
             PERFORM READ-RECEIPTS-NEXT THRU READ-RECEIPTS-EXIT.
           IF WS-F-ERROR = 90
               GO TO DA45.
      *
      *    ****   T E S T   I F   O U T P U T   T O   P R I N T E R
      *           O R   S P O O L   F I L E
      *
           IF NOT(WS-PRINTER = "S")
               DISPLAY RCT-REF AT 1128 WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
             MOVE RCT-AC             TO REC-AC.
             MOVE RCT-NAME           TO REC-NAME.
      *
      *    ****   S W I T C H   D A T E
      *           F R O M   YYYYMMDD   T O   DDMMYYYY
      *
             MOVE RCT-DATE           TO W22-DTE2.
             PERFORM SWITCH-DATE-TO-DMCY THRU SWITCH-DATE-EXIT.
             MOVE W22-DTE3           TO REC-DTE2.
             MOVE RCT-REF            TO REC-REF.
             MOVE RCT-VAL            TO REC-VAL.
             MOVE RCT-CSH            TO REC-CSH.
             MOVE RCT-CHQ            TO REC-CHQ.
             MOVE RCT-TRF            TO REC-TRF.
             ADD RCT-VAL             TO W90-VAL.
             ADD RCT-CSH             TO W90-CSH.
             ADD RCT-CHQ             TO W90-CHQ.
             ADD RCT-TRF             TO W90-TRF.
             MOVE 1                  TO WS-ADVANCE.
           IF W02-LINAGE < W02-PRN-LENGTH
               MOVE 1                TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
           ELSE
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               PERFORM DA20 THRU DA25.
             MOVE SPACES             TO R-DET1.
             GO TO DA40.

       DA45.
            PERFORM CALL-PRINTUTL.
            MOVE "TOTALS"            TO REC-REF.
            MOVE W90-VAL             TO REC-VAL.
            MOVE W90-CSH             TO REC-CSH.
            MOVE W90-CHQ             TO REC-CHQ.
            MOVE W90-TRF             TO REC-TRF.
            MOVE "E"                 TO WS-COMMAND.
            MOVE 99                  TO WS-ADVANCE.
            PERFORM CALL-PRINTUTL.

       DA50.
             MOVE "C"                TO WS-COMMAND.
             PERFORM CALL-PRINTUTL.
             CLOSE RECEIPTS.
             PERFORM ERASE-SCREEN.
             MOVE "Clear Receipts Report File Y/N  [ ]" TO WS-OPT-MES.
             MOVE "Y"                TO WS-OPTION.
             MOVE 1                  TO WS-INSTR.
             PERFORM OPT-MESSAGE.
           IF WS-OPTION = "Y"
               OPEN OUTPUT RECEIPTS
               CLOSE RECEIPTS.

       DA999-EXIT.
             EXIT.
      *    *************************************************************
      *                I N I T I A L I S E   P R O G R A M
      *    *************************************************************
       ZA000-INIT       SECTION 9.
       ZA00-OPEN.
             MOVE LS-PARID           TO WS-PARID.
             MOVE LS-TODAY-DDMMYY    TO TODAY-DDMMYY W12-TODAY.
             MOVE LS-TODAY-YYMMDD    TO W12-T-YMD.
             MOVE LS-USUB            TO WS-USUB.
             MOVE LS-COMPANY         TO CRT-COMPANY.
             MOVE LS-TERMINAL        TO CRT-TERMINAL.
             PERFORM HEADING-AND-CLEAR-SCREEN.      
      *    旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
      *    �  Setup various keys and the mouse for use with menus and  �
      *    �  other required user interface functions and procedures.  �
      *    읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�

             COPY "FUNCTION.PRO".

       ZA25.
             MOVE 4                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-AGE            TO WS-AGE.
             MOVE 1                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-COMPANY        TO W95-COMP.
      *      MOVE PAR-DMY            TO W12-TODAY.
      *      MOVE PAR-YMD            TO W12-T-YMD.
             MOVE 1                  TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
             GO TO ZA999-EXIT.

       ZA200.

       COPY "LOCKED.PRO".

       ZA205.
             EXIT PROGRAM.

       ZA999-EXIT.
             EXIT.
      *
      *    ****    I / O   E R R O R   M E S S A G E S
      *
       ZB000       SECTION 9.

       COPY "ERRORS.PRO".

       DISPLAY-FILE-NAME.
             MOVE ZERO               TO WS-KEY.
             EVALUATE WS-F-ERROR
               WHEN 2          MOVE W02-NETWORK   TO WS-FILE
                               MOVE WS-NETKEY     TO WS-KEY
               WHEN 15         MOVE WS-PARID      TO WS-FILE
                               MOVE WS-PARKEY     TO WS-KEY
               WHEN 90         MOVE W02-RECEIPTS  TO WS-FILE
                               MOVE RCT-REF       TO WS-KEYX
               WHEN OTHER      MOVE "ERROR"       TO WS-FILE
                               MOVE "DTP019"      TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
