      $SET LINKCOUNT"384" GNT"DTP013.GNT"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******     ****       **      ******    *
      *   **    **     **     **    **   **  **     ***     **    **   *
      *   **    **     **     **    **  **    **     **           **   *
      *   **    **     **     *******   **    **     **        ****    *
      *   **    **     **     **        **    **     **           **   *
      *   **    **     **     **         **  **      **     **    **   *
      *   *******      **     **          ****     ******    ******    *
      *                                                                *
      *       ENGLISH                                                  *
      *                                                                *
      *          A C C O U N T S   R E C E I V A B L E /               *
      *                                                                *
      *       D E B T O R   A L L O C A T E   C R E D I T S            *
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
       PROGRAM-ID.     DTP013.
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

       COPY "DBTRAN.SL".

       COPY "DEBTOR.SL".

       COPY "DEBTRN.SL".

       COPY "PARAM.SL".

       COPY "RECOVER.SL".

       COPY "SHARED.SL".

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

       COPY "DBTRAN.FDE".

       COPY "DEBTOR.FDE".

       COPY "DEBTRN.FD".

       COPY "PARAM.FDE".

       COPY "RECOVER.FD".

       COPY "SHARED.FDE".

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
       77  WS-SUB                  PIC  9(04)    COMP-5.
       77  WS-S                    PIC  9(04)    COMP-5.
       77  WS-S1                   PIC  9(04)    COMP-5.
       77  WS-S2                   PIC  9(04)    COMP-5.
       77  WS-S3                   PIC  9(04)    COMP-5.
       77  WS-S4                   PIC  9(04)    COMP-5.
       77  WS-S5                   PIC  9(04)    COMP-5.
       77  WS-S6                   PIC  9(04)    COMP-5.
       77  WS-S7                   PIC  9(04)    COMP-5.
       77  WS-S8                   PIC  9(04)    COMP-5.
       77  WS-S9                   PIC  9(04)    COMP-5.
       77  WS-IXS1                 PIC  9(04)    COMP-5.
       77  WS-IXS2                 PIC  9(04)    COMP-5.
       77  WS-IXS3                 PIC  9(04)    COMP-5.
       77  WS-IXS4                 PIC  9(04)    COMP-5.
       77  WS-DBTKEY               PIC  9(06)    COMP-5.
       77  WS-ENQPOS               PIC  9(06)    COMP-5.
       77  WS-ENQEND               PIC  9(06)    COMP-5.
       77  WS-NETKEY               PIC  9(06)    COMP-5.
       77  WS-PARKEY               PIC  9(06)    COMP-5.
       77  WS-RECKEY               PIC  9(06)    COMP-5.
       77  WS-RECOVER              PIC  9(06)    COMP-5.
       77  WS-SHARED               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-TRANS                PIC  9(04)    COMP-5 VALUE 1.
       77  WS-PERIOD               PIC  9(02).
       77  WS-NTP                  PIC  X(01).
       77  WS-KEYSTR               PIC  9(06)    COMP-5.
       77  WS-OPTION               PIC  X(01).
       77  WS-ALLOCATE             PIC  X(01)           VALUE "M".
       77  WS-SKIP                 PIC  X(01)           VALUE "N".
       77  WS-TAX                  PIC  X(01).
       77  WS-TYPE                 PIC  X(01).
       77  WS-ERROR                PIC  9(01)           VALUE ZERO.
       77  WS-EKEY                 PIC  9(06)    COMP-5.
       77  WS-CHAR1                PIC  X(01).
       77  WS-CHAR2                PIC  X(01).
       77  WS-COMP                 PIC  9(01).
       77  WS-BATCH                PIC  9(06)    COMP-5.
       77  WS-ERR1                 PIC  X(22)           VALUE "Invalid Account Number".
       77  WS-ERR2                 PIC  X(09)           VALUE "No Record".
       77  WS-E                    PIC  X(15)           VALUE "**END** - ENTER".
       77  WS-M                    PIC  X(17)           VALUE "New batch (Y/N) ?".
       77  WS-AC                   PIC  X(07)           VALUE "ACCOUNT".
       77  PRG-DEBT-LUP            PIC  X(12)           VALUE "DTP\DTPLOOK".
       77  TODAY-DDMMYY            PIC  9(08)    COMP-5.
       77  WS-USUB                 PIC  9(04)    COMP-5.
      *
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
           03  CRT-START           PIC  9(02) VALUE 5.
           03  CRT-END             PIC  9(02) VALUE 46.
      *
      *    Calling Program
      *
           03  CRT-PROGRAM         PIC  X(15) VALUE "DTP013".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "ACCOUNTS RECEIVABLE -ALLOCATE CREDITS".
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
           03  WS-MODULE       PIC  X(03) VALUE "DTP".
           03  WS-PROG        PIC  X(03) VALUE "013".

       01  W00-TRAN-CODES.
           03  W00-T-VAT           PIC  9(09)V99 COMP-3.
           03  W00-E-DESC          PIC  X(12).
           03  W00-A-DESC          PIC  X(12).
           03  W00-ACTION          PIC  9(01).
           03  W00-T-CODE          PIC  9(02)    COMP-5.
           03  W00-T-GL            PIC  9(06)    COMP-3.
           03  W00-T-DAY           PIC  9(06)    COMP-5.
           03  W00-T-VAL           PIC S9(09)V99 COMP-3.
           03  W00-T-MTD           PIC  9(06)    COMP-5.
           03  W00-T-MTDV          PIC S9(09)V99 COMP-3.
           03  W00-T-YTD           PIC  9(06)    COMP-5.
           03  W00-T-YTDV          PIC S9(09)V99 COMP-3.

       01  WS-PARID.
          03  WS-SYS-ID       PIC  X(03).

       01  W10-SPARES.
          03  W10-DATE     PIC X(10).
          03  W10-EDTE REDEFINES W10-DATE PIC 99/99/9999.

       01  W10-REF.
           03  W10-NUM         PIC  9(08).

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
          03  W25-RESULT      PIC  9(05)V99.
           03  W25-RESULT1 REDEFINES W25-RESULT.
              05  W25-WHOLE   PIC  9(05).
              05  W25-DECIMAL PIC  9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
        05  W25-KEY     PIC  9(04).
        05  W25-SUB     PIC  9(01).
        05  FILLER      PIC  9(02).
           03  W25-TOTAL       PIC S9(09)V99 COMP-3.
           03  W25-VALUE       PIC S9(09)V99 COMP-3.

       COPY "W40.DBT".

       COPY "W40.WS".

       01  W45-TRAN.
           03  W45-CODE        PIC 9(02).
           03  W45-ENG         PIC X(12).
           03  W45-AFR         PIC X(12).
           03  W45-ACT         PIC X(01).

       COPY "FUNCTION.WS".

      *COPY "W50.WS".

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
           03  W75-S               PIC  9(02)    COMP-5.
           03  W75-S1              PIC  9(02)    COMP-5.
           03  W75-KEY             PIC  9(06)    COMP-5.
           03  W75-ALLOCATED       PIC S9(09)V99 COMP-3.
           03  W75-BALANCE         PIC S9(09)V99 COMP-3.
           03  W75-DNO             PIC  X(06)    OCCURS 18.

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
           03  W80-ACNO            PIC X(06).
      *    03  W80-TYPE            PIC 9(01).

       01  W85-PASS.
           03  W85-SUPER           PIC  X(06)    OCCURS 9.
           03  W85-ENTRY           PIC  9(02)    COMP-3.
           03  W85-MARG            PIC S9(02)V99.

       01  W85-BATCHES.
               05  W85-BATCH       PIC  9(06)    COMP-5.

       COPY "W90.DBT".

       01  W91-TRAN.
           03  W91-KEY.
               05  W91-AC      PIC  X(06).
               05  W91-DATE    PIC  9(08)    COMP.
               05  W91-CODE    PIC  9(02).
               05  W91-REF     PIC  X(08).
      *
      *    TYPE: "U" = Unpaid.         \
      *          "P" = Paid.            >  DEBIT TRANSACTIONS
      *          "X" = Partial payment./
      *
      *          "U" = Unallocated.    \
      *          "A" = Allocated.       >  CREDIT TRANSACTIONS
      *          "X" = Partial allocat./
      *
           03  W91-TYPE        PIC  X(01).
           03  W91-REG         PIC  X(09).
           03  W91-ORD         PIC  X(12).
           03  W91-VALUE       PIC S9(09)V99 COMP-3.
           03  W91-TAXFREE     PIC S9(09)V99 COMP-3.
           03  W91-TAX         PIC S9(07)V99 COMP-3.
           03  W91-DISC        PIC S9(03)V99 COMP-3.
           03  W91-PAID        PIC S9(09)V99 COMP-3.
           03  W91-ALLOCATED REDEFINES W91-PAID
                               PIC S9(09)V99 COMP-3.
           03  W91-REC         PIC  X(08).
           03  W91-AGE         PIC  9(02)    COMP-5.
           03  FILLER          PIC  X(01).

       01  W95-STM.
           03  W95-COMP        PIC X(40).
           03  W95-V2.
               05  W95-B       PIC Z(06)9.99-.

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

       COPY "DEBLUP.CRT".

       01  S08A.
           03  LINE 16 COLUMN  6 VALUE "Allocation:".
           03  LINE 17 COLUMN  6 VALUE "Ref. No.    Date    Transaction         Value      Balance     Allocate".

       01  S36.
           03  PIC X(06) USING W80-ACNO FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta AUTO.

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
       AA000         SECTION.
       AA00.
           IF LS0-DBLEV < 2
      *
      *      Insufficient security level
      *
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
               GO TO AA49.
             PERFORM ZA000-INIT.
             PERFORM CL000.
             CLOSE RECOVER.
       AA49.
             EXIT PROGRAM.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ This routine is used to call the Debtor / Ac's Receivable ³
      *    ³ lookup program and cancel it when control is returned.    ³
      *    ³ The  relevant  error  message  will be  displayed  if  an ³
      *    ³ exception occurs. The usual  parameters are passed to the ³
      *    ³ lookup program as well W40-COMPANY which will contain the ³
      *    ³ details of the account that has been selected when control³
      *    ³ is returned to the calling program.                       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AA50-LOOKUP       SECTION.
       AA50.
             PERFORM SAVE-SCREEN.
      *      MOVE 4                  TO W44-FUNCTION.
             PERFORM SCREEN-CONTENTS.
             MOVE PRG-DEBT-LUP       TO PRG-NAME.
             CALL PRG-NAME USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY DATA-FILE-IDS W40-COMPANY
                  ON EXCEPTION GO TO AA55
             END-CALL.
             CANCEL PRG-NAME.
             MOVE W40-ACNO           TO W80-ACNO.
             GO TO AA58.

       AA55.
             MOVE SPACE              TO WS-ERR-MES.
             STRING "Program- " DELIMITED SIZE PRG-NAME DELIMITED " " " not on disk, press ANY key" DELIMITED SIZE INTO WS-ERR-MES.
             PERFORM ERROR-MESSAGE.

       AA58.
             PERFORM RESTORE-SCREEN.

       AA99.
             EXIT.

       COPY "AA900.ALM".

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

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                        OPT-MESSAGE                        ³
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
      *    ³ To clear line 50:                                         ³
      *    ³    PERFORM CLEAR-L50                                      ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "CLEAR.CRT".

       COPY "DEBTOR.TRN".

      *
      *    ****    READ FILES ROUTINES
      *
       AC000              SECTION.

       COPY "CONTROL.RD".

       COPY "DBTRAN.RD1".

       COPY "DEBTOR.RD".

       COPY "DEBTRN.RD".

       COPY "PARAM.RD".

       COPY "SHARED.RD".

      *
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
       AD000             SECTION.

       COPY "CONTROL.WR".

       COPY "DBTRAN.WR".

       COPY "DEBTOR.WR".

       COPY "DEBTRN.WR".

       COPY "PARAM.WR".

       COPY "DEBTOR.LUP".

       COPY "APAC.HLP".

      *    *************************************************************
      *          THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *    *************************************************************
       AY000           SECTION.
      *    *************************************************************
      *    ****             P A R A M E T E R   F I L E             ****
      *    *************************************************************
       AY01.
             MOVE 01                 TO REC-FILE.
             MOVE WS-PARKEY          TO REC-KEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-RECORD1        TO REC-PARAM.
             GO TO AY50.
             
       AY02.
             MOVE 02                 TO REC-FILE.
             MOVE ZERO               TO REC-KEY.
             MOVE WS-ACTION          TO REC-TYPE
           IF WS-ACTION = 0 OR 2
               PERFORM READ-DEBTOR-LOCK THRU READ-DEBTOR-EXIT.
             MOVE DEB-RECORD1        TO REC-DEBTOR.
             GO TO AY50.
             
       AY04.
             MOVE 04                 TO REC-FILE.
             MOVE ZERO               TO REC-KEY.
             MOVE WS-ACTION          TO REC-TYPE.
             MOVE TRA-RECORD1        TO REC-DBTRAN.
             GO TO AY50.

       AY39.
             MOVE 39                 TO REC-FILE.
             MOVE WS-NETKEY          TO REC-KEY.
             MOVE NET-RECORD         TO REC-NETWORK.
             GO TO AY50.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³              End transaction in recovery log              ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AY49.
             MOVE 99                 TO REC-FILE.
             MOVE ZERO               TO REC-KEY.
             MOVE SPACES             TO REC-DETAIL.
             PERFORM AY50 THRU AY59.
             ADD 1                   TO WS-TRANS.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ If recovery records exceed 95 - Clear (Close and Open     ³
      *    ³ new). This allows for quicker recoveries when required.   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
           IF WS-RECOVER > 95
               CLOSE RECOVER
               OPEN OUTPUT RECOVER
               CLOSE RECOVER
               OPEN I-O RECOVER
               MOVE ZERO             TO WS-RECOVER.
             GO TO AY59.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³         W R I T E   R E C O V E R Y   R E C O R D         ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AY50.
             ADD 1                   TO WS-RECOVER.
             MOVE WS-RECOVER         TO WS-RECKEY.
             MOVE WS-TRANS           TO REC-TRAN.
             WRITE REC-RECORD.
           IF WS-STATUS NOT = "00"
               DISPLAY "Write error Recovery file - Status " AT 4512 WITH FOREGROUND-COLOR Brown HIGHLIGHT
                        WS-STATUS                                    WITH FOREGROUND-COLOR Grey  HIGHLIGHT
               STOP RUN.
             CLOSE RECOVER.
             OPEN I-O RECOVER.

       AY59.
             EXIT.
      *    *************************************************************
      *    ****    Start processing transaction
      *    *************************************************************
       AY60.
             MOVE 1                  TO WS-COUNT.
             MOVE 5                  TO WS-SHARED.
             PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT.
      *    *************************************************************
      *    ****    Read PARAM record 4 - Lock record and start recovery
      *            file procedure.
      *    *************************************************************
             MOVE 4                  TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
           IF PAR-USERS < 24
               MOVE 1                TO WS-SUB
               MOVE ZERO             TO WS-WAIT
               GO TO AY62.
      *    *************************************************************
      *    ****   Q   F U L L  -  W A I T   F O R   4 S E C O N D S
      *    *************************************************************
             DISPLAY "WAITING" AT 5051 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Red HIGHLIGHT.
             COMMIT.
             ACCEPT WS-STIME FROM TIME.
             MOVE 400                TO WS-WAIT.
             PERFORM LOCK-REC-LOOP.
             DISPLAY SPACE AT 5051     WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Cyan.
             GO TO AY60.

       AY61.
             MOVE "DB"               TO PAR-PROG(WS-USUB).
             MOVE LS-USER            TO PAR-USR(WS-USUB).
             PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      *    *************************************************************
      *    ****    Read the DEBTOR control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *    *************************************************************
             MOVE 1                  TO WS-NETKEY.
             PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
             PERFORM AY39 THRU AY59.
             GO TO AY999.
      *    *************************************************************
      *    ****   A R E   A N Y   U P D A T E S   I N   P R O G R E S S
      *    *************************************************************
       AY62.
           IF NOT(PAR-PROG(WS-SUB) = SPACES OR PAR-USR(WS-SUB) = SPACES)
      *    *************************************************************
      *    ****   A R E   D E B T O R   O R   S T O C K   F I L E S
      *                     B E I N G   U P D A T E D
      *    *************************************************************
               IF NOT(PAR-PROG(WS-SUB) = SPACES)
                   IF PAR-PROG(WS-SUB) = "DB" OR "DS"
      *    *************************************************************
      *    ****   Y E S   -   S E T   W A I T   P E R I O D
      *    *************************************************************
                       GO TO AY62-WAIT
                   ELSE
                       ADD 1         TO WS-SUB
                       GO TO AY62
                   END-IF
               ELSE
      *    *************************************************************
      *    ****   I S   T H I S   P R O G R A M   I N   T H E   Q
      *    *************************************************************
               IF PAR-USR(WS-SUB) = LS-USER
      *    *************************************************************
      *    ** I S   I T   N E X T   I N   L I N E   T O   P R O C E S S
      *    *************************************************************
                   IF WS-WAIT = ZERO
                       GO TO AY63
                   ELSE
                       GO TO AY62-WAIT
                   END-IF
               ELSE
                   GO TO AY62-WAIT
               END-IF
           END-IF.
             MOVE LS-USER            TO PAR-USR(WS-SUB).
             MOVE WS-SUB             TO PAR-USERS.
             PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
             GO TO AY62-CHECK.
      *    *************************************************************
      *    ****   S E T   W A I T   P E R I O D
      *    *************************************************************
       AY62-WAIT.
             MOVE 300                TO WS-WAIT.
          IF NOT(PAR-USR(WS-SUB) = LS-USER)
              IF WS-SUB < 24
                  ADD 1              TO WS-SUB
                  GO TO AY62.

       AY62-CHECK.
           IF WS-WAIT > ZERO
               COMMIT
               DISPLAY "Waiting" AT 5072 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT BLINK
               ACCEPT WS-STIME FROM TIME
               PERFORM LOCK-REC-LOOP
               DISPLAY "Waiting" AT 5072 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT BLINK
               GO TO AY60.
             DISPLAY SPACE AT 5072 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Cyan.

       AY63.
             MOVE WS-SUB             TO WS-USUB.
             GO TO AY61.

       AY70.
             MOVE 4                  TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK-END THRU READ-PARAM-EXIT.
      *    *************************************************************
      *    ****    Write links back and unlock PARAM record 4 and
      *                          NETWORK record 2.
      *    *************************************************************
             PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
             PERFORM AY49 THRU AY59.
             MOVE 1                  TO WS-USUB.

       AY72.
           IF NOT(PAR-USR(WS-USUB) = LS-USER)
               IF WS-USUB < 24
                   ADD 1             TO WS-USUB
                   GO TO AY72
               ELSE
                   GO TO AY85.

       AY75.
             MOVE SPACES             TO PAR-PROG(WS-USUB) PAR-USR(WS-USUB).
           IF WS-USUB < 24
               ADD 1 WS-USUB  GIVING WS-SUB
           ELSE
               GO TO AY80.
           IF NOT(PAR-PROG(WS-SUB) = SPACES)
               MOVE PAR-PROG(WS-SUB) TO PAR-PROG(WS-USUB)
               MOVE PAR-USR(WS-SUB)  TO PAR-USR(WS-USUB)
               ADD 1                 TO WS-USUB
               GO TO AY75.

       AY80.
             SUBTRACT 1 FROM WS-USUB GIVING PAR-USERS.
             PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.

       AY85.
             COMMIT.

       AY999.
             EXIT.
      *
      *    ****    Read debtor record - using account number.
      *
       CA155-GET-DEBTOR  SECTION 2.
       CA155.
             MOVE W80-ACNO       TO DEB-ACNO.
             PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 6
               MOVE 1            TO WS-ERROR
           ELSE
           IF DEB-ACNO NOT = W80-ACNO
               MOVE 2            TO WS-ERROR
           ELSE
               MOVE 0            TO WS-ERROR
      *        MOVE DEB-TYPE     TO W80-TYPE
      *
      *    ****    Move the Debtor Record to working storage area
      *
               MOVE DEB-RECORD1      TO W40-DEBTREC.

       CA160-EXIT.
             EXIT.

       COPY "DEBTOR.ALL".

      *
      *    ****    A L L O C A T E   C R E D I T S
      *
       CL000        SECTION 5.
       CL00.
             PERFORM ERASE-SCREEN.
             DISPLAY "CREDIT TRANSACTION ALLOCATION" AT 0226
                      WITH FOREGROUND-COLOR Grey HIGHLIGHT.

             DISPLAY "Account :" AT 0602.
             MOVE SPACES         TO W80-ACNO.
       CL05.
             DISPLAY "F1"                 AT 5002 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                     "=Help,"                     WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                     "F2"                         WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                     " to do Debtor Lookup "      WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                     "F9"                         WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                     " Debtor Lookup (Any Word)," WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                     "Esc"                        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                     " to exit"                   WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
             ACCEPT S36 AT 0612.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    MOVE SPACES TO W80-ACNO
                                 GO TO CL999
                 WHEN F1-KEY     PERFORM HELP-ROUTINE
                 WHEN F2-KEY     MOVE SPACE  TO W40-LUP
                                 PERFORM AA50
                 WHEN F9-KEY     MOVE "A"    TO W40-LUP
                                 PERFORM AA50
                 WHEN OTHER      PERFORM AA900-ALARM
                                 GO TO CL05
               END-EVALUATE
               DISPLAY S36 AT 0612
               IF W80-ACNO = SPACES
                   GO TO CL05.
             PERFORM CLEAR-L50.
           IF W80-ACNO = SPACES
               GO TO CL999.
             MOVE 6                  TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W80-ACNO BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 1                  TO WS-LENGTH.
             MOVE W80-ACNO           TO DEB-ACNO.
             PERFORM CA155-GET-DEBTOR.
           IF WS-ERROR = 1
              MOVE WS-ERR2           TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO CL05.
           IF WS-ERROR = 2
              MOVE WS-ERR1           TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO CL05.
      *    *************************************************************
      *    ****         T E S T   F O R   I N D I V I D U A L       ****
      *    *************************************************************
           IF W40-TYPE = 1
               MOVE "D"              TO W40-INSTR
               CALL "DTP\DTPNAME" USING W40-COMPANY LS-USER-ID
           ELSE
               MOVE W40-NAME         TO W40-DSP-NAME.
             DISPLAY W40-DSP-NAME AT 0619 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               GO TO CL00.
           IF NOT OPEN-ITEM
               MOVE "Not an OPEN ITEM account" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO CL05.
             DISPLAY "Date"                                         AT 0805
                              "Cde/description"                     AT 0813
                                              "Ref. No."            AT 0829
                                                            "Value" AT 0843.
             MOVE DEB-ACNO           TO TRA-KEY.

       CL08.
             PERFORM START-AT-DTRN-KEY THRU READ-DBTRAN-EXIT.
           IF WS-F-ERROR = 5
               GO TO CL35.

       CL10.
             PERFORM READ-DBTRAN-NEXT THRU READ-DBTRAN-EXIT.
           IF (WS-F-ERROR = 5) OR (TRA-AC NOT = DEB-ACNO)
               GO TO CL35.
           IF TRA-VALUE NOT < ZERO
               GO TO CL10.
           IF TRA-VALUE < ZERO
               IF (TRA-VALUE + TRA-ALLOCATED) = ZERO
                   GO TO CL10.
             MOVE TRA-CODE           TO W90-CODE.
             PERFORM GET-DEBTOR-TRAN-DESC.
             MOVE TRA-DATE           TO W22-DTE2.
             MOVE W22-CC2            TO W22-CC3.
             MOVE W22-YY2            TO W22-YY3.
             MOVE W22-MM2            TO W22-MM3.
             MOVE W22-DD2            TO W22-DD3.
             MOVE W22-DTE3           TO W90-DTE.
             MOVE TRA-REF            TO W90-REF.
             MOVE TRA-VALUE          TO W95-B.
             DISPLAY W90-DTE    AT 0902 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
             DISPLAY W90-CODE   AT 0913 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
             DISPLAY W00-E-DESC AT 0916 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
             DISPLAY TRA-REF    AT 0929 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
             DISPLAY W95-V2     AT 0938 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
             COMPUTE W90-CR = (TRA-VALUE + TRA-ALLOCATED) * -1.
             MOVE TRA-RECORD1        TO W91-TRAN.
             MOVE ZERO               TO W90-ALLOCATED W90-DISCOUNT.
             DISPLAY S08A.
             MOVE 19                 TO CLIN.
             PERFORM ALLOCATE-CREDIT.
           IF (WS-ERROR = 9) OR (W75-ALLOCATED = W91-ALLOCATED)
               GO TO CL30.
             PERFORM AY60 THRU AY999.
             MOVE 0                  TO WS-ACTION.
             PERFORM AY02 THRU AY59.
             MOVE ZERO               TO W90-CUR W90-30 W90-60 W90-90 W90-120 W90-INT.

       CL15.
           IF W75-KEY < WS-DBTKEY
               GO TO CL25.
             PERFORM READ-DEBTRN THRU READ-DEBTRN-EXIT.
           IF DBT-UPDATE = SPACE
               GO TO CL20.
             ADD DBT-ALLOC           TO DBT-PAID.
           IF DBT-VALUE > DBT-PAID
               MOVE "X"              TO DBT-TYPE
           ELSE
               MOVE "P"              TO DBT-TYPE.
           IF DBT-CODE = 20
               SUBTRACT DBT-ALLOC    FROM DEB-INT
               ADD DBT-ALLOC         TO W90-INT
           ELSE
               EVALUATE DBT-AGE
                 WHEN 1     SUBTRACT DBT-ALLOC FROM DEB-CUR
                            ADD DBT-ALLOC      TO W90-CUR
                 WHEN 2     SUBTRACT DBT-ALLOC FROM DEB-30
                            ADD DBT-ALLOC      TO W90-30
                 WHEN 3     SUBTRACT DBT-ALLOC FROM DEB-60
                            ADD DBT-ALLOC      TO W90-60
                 WHEN 4     SUBTRACT DBT-ALLOC FROM DEB-90
                            ADD DBT-ALLOC      TO W90-90
                 WHEN 5     SUBTRACT DBT-ALLOC FROM DEB-120
                            ADD DBT-ALLOC      TO W90-120
               END-EVALUATE.
             MOVE DBT-KEY            TO TRA-KEY.
             PERFORM READ-DBTRAN THRU READ-DBTRAN-EXIT.
             MOVE 0                  TO WS-ACTION.
             PERFORM AY04 THRU AY59.
             MOVE DBT-TRA            TO TRA-RECORD1.
             MOVE W90-REF            TO TRA-REC.
             PERFORM REWRITE-DBTRAN THRU WRITE-DBTRAN-EXIT.

       CL20.
             ADD 1                   TO WS-DBTKEY.
             GO TO CL15.

       CL25.
             MOVE W91-TRAN           TO TRA-RECORD1.
             MOVE 0                  TO WS-ACTION.
             PERFORM AY04 THRU AY59.
             ADD W75-ALLOCATED       TO TRA-ALLOCATED.
             PERFORM REWRITE-DBTRAN THRU WRITE-DBTRAN-EXIT.
             PERFORM REWRITE-DEBTOR THRU WRITE-DEBTOR-EXIT.
             SUBTRACT W90-120        FROM DEB-FOUR.
             SUBTRACT W90-90         FROM DEB-THREE.
             SUBTRACT W90-60         FROM DEB-TWO.
             SUBTRACT W90-30         FROM DEB-ONE.
             SUBTRACT W90-CUR        FROM DEB-MTD.
             SUBTRACT W90-INT        FROM DEB-INTEREST.
             PERFORM AY70 THRU AY999.

       CL30.
             CLOSE DEBTRN.

       CL32.
             MOVE "'C'ontinue or 'E'xit  [C]" TO WS-OPT-MES.
             MOVE "C"                TO WS-OPTION.
             MOVE "V"                TO WS-INSTR.
             PERFORM OPT-MESSAGE.
           IF WS-OPTION = "E"
               COMMIT
               GO TO CL00.
             MOVE W91-KEY            TO TRA-KEY.
             PERFORM START-AT-DTRN-KEY THRU READ-DBTRAN-EXIT.
           IF WS-F-ERROR = 5
               GO TO CL35.
             PERFORM READ-DBTRAN-NEXT THRU READ-DBTRAN-EXIT.
             GO TO CL10.

       CL35.
             COMMIT.
             MOVE "No credits to allocate" TO WS-ERR-MES.
             PERFORM ERROR-MESSAGE.
             GO TO CL00.

       CL999.
             EXIT.
      *    *************************************************************
      *                I N I T I A L I S E   P R O G R A M
      *    *************************************************************
       ZA000-INIT        SECTION 8.
       ZA000-OPEN.
             MOVE LS-PARID           TO WS-PARID.
             MOVE LS-TODAY-DDMMYY    TO TODAY-DDMMYY W12-TODAY.
             MOVE LS-TODAY-YYMMDD    TO W12-T-YMD.
             MOVE LS-USUB            TO WS-USUB.
             MOVE LS-COMPANY         TO CRT-COMPANY.
             MOVE LS-TERMINAL        TO CRT-TERMINAL.
             PERFORM HEADING-AND-CLEAR-SCREEN.      
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

             COPY "FUNCTION.PRO".

       ZA02.
             OPEN OUTPUT RECOVER.
             CLOSE RECOVER.
             OPEN I-O RECOVER.
             MOVE 1                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-COMPANY        TO W95-COMP.
      *      MOVE PAR-DMY            TO W12-TODAY.
      *      MOVE PAR-YMD            TO W12-T-YMD.

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
               WHEN 2          MOVE W02-NETWORK   TO WS-FILE
                               MOVE WS-NETKEY     TO WS-KEY
               WHEN 5          MOVE W02-DBTRAN    TO WS-FILE
                               MOVE TRA-KEY       TO WS-KEYX
               WHEN 6          MOVE W02-DEBTOR    TO WS-FILE
                               MOVE DEB-ACNO      TO WS-KEYX
               WHEN 15         MOVE WS-PARID      TO WS-FILE
                               MOVE WS-PARKEY     TO WS-KEY
               WHEN 18         MOVE W02-RECOVER   TO WS-FILE
                               MOVE WS-RECKEY     TO WS-KEY
               WHEN 31         MOVE W02-DEBTRN    TO WS-FILE
                               MOVE WS-DBTKEY     TO WS-KEY
               WHEN 37         MOVE W02-SHARED    TO WS-FILE
                               MOVE WS-SHARED     TO WS-KEY
               WHEN OTHER      MOVE "ERROR"       TO WS-FILE
                               MOVE "DTP013"      TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
