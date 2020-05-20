      $SET LINKCOUNT"384" GNT"DTP001.GNT"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******     ****      ****       **      *
      *   **    **     **     **    **   **  **    **  **     ***      *
      *   **    **     **     **    **  **    **  **    **     **      *
      *   **    **     **     *******   **    **  **    **     **      *
      *   **    **     **     **        **    **  **    **     **      *
      *   **    **     **     **         **  **    **  **      **      *
      *   *******      **     **          ****      ****     ******    *
      *                                                                *
      *       ENGLISH                                                  *
      *                                                                *
      *          A C C O U N T S   R E C E I V A B L E   /             *
      *                                                                *
      *      D E B T O R   C R E A T I O N S   P R O G R A M           *
      *                                                                *
      *       Version 9.04.01 - May 2016                               *
      *                                                                *
      ******************************************************************
      *                                                                *
      *  May 2006 - New field included in Accounts Receivable for      *
      *             recording number of outstanding Jobs linked to the *
      *             account. Additional filler added for future        *
      *             expansion.                                         *
      *                                                                *
      *  Feb 2007 - New file (DEBDEP/AR_REST.DAT) - Debtor sales       *
      *             restrictions file has been included which will     *
      *             allow for departments to be excluded or only       *
      *             selected departments allowed. This will allow the  *
      *             user the abillity to allow or bar sales of select- *
      *             ed departments.                                    *
      *                                                                *
      *  Jan 2008 - New file (DEBALT) - Debtor alternate index includ- *
      *             ed for lookups, using any word contained in the    *
      *             Debtor name.                                       *
      *                                                                *
      *  July 2012 - Added email address field - DEB-EMAIL             *
      *              (W40-EMAIL) PIC X(64)                             *
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
      *             Changed DEB-NAME from 40 characters to a group     *
      *             name containing the following fields: DEB-SNAME,   *
      *             DEB-1ST-NAME and DEB-2ND-NAME each defined as PIC  *
      *             X(24) as well as DEB-INIT PIC X(08)(W40-SURNAME,   *
      *             W40-FNAME, W40-FNAME and W40-INITIALS). This allows*
      *             for a name of 80 characters although the screen    *
      *             layout (due to the fact that the system is using   *
      *             an 80 character width screen) for a Company and    *
      *             Internal accounts is providing for only 60 charact-*
      *             ers. This allows an additional 20 characters for   *
      *             Company and Internal account names.                *
      *                                                                *
      *  Jan 2013 - Program DTPMEMO introduced to handle memo lookups  *
      *             reducing code in all programs and removing file    *
      *             DEBMEM from programs. Copy file 'DEBMEM.PRO' now   *
      *             copied into all programs that require lookups for  *
      *             memo/remarks on a Debtor account. Move the Debtor  *
      *             code/account number to W80-ACCOUNT and then        *
      *             PERFORM DEBTOR-MEMO. The Debtor lookup program -   *
      *             DTPLOOK now includes a Memo Lookup so unless there *
      *             is a requirement to lookup memo details after this *
      *             lookup, the MEMO lookup can be removed from all    *
      *             DEBTOR programs.                                   *
      *                                                                *
      *  May 2013 - Added Cell phone number field DEB-C-TEL            *
      *             (W40-C-TEL) PIC X(14).                             *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     DTP001.
       AUTHOR.         J W LEMMON (APAC).
       DATE-WRITTEN.   March 1982.

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

       COPY "DEBALT.SL".

       COPY "DEBTOR.SL".

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

       COPY "DEBALT.FDE".

       COPY "DEBTOR.FDE".

       COPY "PARAM.FDE".

       COPY "RECOVER.FD".

       COPY "SHARED.FDE".

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
       77  WS-CHECK                PIC  X(18)           VALUE "aeWlimemnomLalismJ".
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
       77  WS-ENQPOS               PIC  9(06)    COMP-5.
       77  WS-ENQEND               PIC  9(06)    COMP-5.
       77  WS-NETKEY               PIC  9(06)    COMP-5.
       77  WS-PARKEY               PIC  9(06)    COMP-5.
       77  WS-RECKEY               PIC  9(06)    COMP-5.
       77  WS-RECOVER              PIC  9(06)    COMP-5.
       77  WS-SHARED               PIC  9(04)    COMP-5  VALUE 1.
       77  WS-TRANS                PIC  9(04)    COMP-5  VALUE 1.
       77  WS-FIN-RATE             PIC S9(03)V99 COMP-3.
       77  WS-PERIOD               PIC  9(02).
       77  WS-NTP                  PIC  X(01).
       77  WS-KEYSTR               PIC  9(06)    COMP-5.
       77  WS-INSTR                PIC  X(01).
       77  WS-OPTION               PIC  X(01).
       77  WS-ASSIGN               PIC  X(01)            VALUE "N".
       77  WS-SKIP                 PIC  X(01)            VALUE "N".
       77  WS-TAX                  PIC  X(01).
       77  WS-TYPE                 PIC  X(01).
       77  WS-ERROR                PIC  9(01)            VALUE ZERO.
       77  WS-EKEY                 PIC  9(06)    COMP-5.
       77  WS-CHAR1                PIC  X(01).
       77  WS-CHAR2                PIC  X(01).
       77  WS-COMP                 PIC  9(01).
       77  WS-RT-HD                PIC  X(10)            VALUE "-Retail   ".
       77  WS-WS-HD                PIC  X(10)            VALUE "-Wholesale".
       77  WS-CS-HD                PIC  X(10)            VALUE "-Cash sale".
       77  WS-ERR1                 PIC  X(22)            VALUE "Invalid Account Number".
       77  WS-ERR2                 PIC  X(09)            VALUE "No Record".
       77  WS-ERR3                 PIC  X(15)            VALUE "Data incomplete".
       77  WS-ERR4                 PIC  X(13)            VALUE "Invalid title".
       77  WS-ERR8                 PIC  X(16)            VALUE "Invalid category".
       77  WS-AC                   PIC  X(07)            VALUE "ACCOUNT".
       77  PRG-NAME                PIC  X(12)            VALUE SPACES.
       77  PRG-DEBT-LUP            PIC  X(12)            VALUE "DTP\DTPLOOK".
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
      *         T = Change the time
      *
           03  CRT-TYPE            PIC  X(01).
      *
      *    The following two fields are used for clearing a portion of
      *    the screen. (Start line and end line)
      *
           03  CRT-START           PIC  9(02)            VALUE 5.
           03  CRT-END             PIC  9(02)            VALUE 46.
      *
      *    Calling Program
      *
           03  CRT-PROGRAM         PIC  X(15)            VALUE "DTP001".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40)            VALUE "ACCOUNTS RECEIVABLE - ADD ACCOUNT".
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

      *

       COPY "WS.WS".

       01  WS-HELP.
           03  WS-MODULE           PIC  X(03)            VALUE "DTP".
           03  WS-PROG             PIC  X(03)            VALUE "001".

       01  WS-PARID.
           03  WS-SYS-ID           PIC  X(03).

       01  W10-SPARES.
           03  W10-DATE            PIC  X(08).
           03  W10-RDATE                                 REDEFINES W10-DATE.
               05  W10-EDTE        PIC  99/99/9999.

       01  W10-REF.
           03  W10-NUM             PIC  9(08).

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
           03  W25-RESULT1                               REDEFINES W25-RESULT.
               05  W25-WHOLE       PIC  9(05).
               05  W25-DECIMAL     PIC  9(02).
           03  W25-RESULT2                               REDEFINES W25-RESULT.
               05  W25-KEY         PIC  9(04).
               05  W25-SUB         PIC  9(01).
               05  FILLER          PIC  9(02).
           03  W25-TOTAL           PIC S9(09)V99 COMP-3.
           03  W25-VALUE           PIC S9(09)V99 COMP-3.

       01  W30-AGE-DATES.
           03  W30-120             PIC  9(08)    COMP-3.
           03  W30-90              PIC  9(08)    COMP-3.
           03  W30-60              PIC  9(08)    COMP-3.
           03  W30-30              PIC  9(08)    COMP-3.
           03  W30-CUR             PIC  9(08)    COMP-3.

       COPY "W40.DBT".

       01  W40-BALANCES.
           03  W40-A-MTD           PIC S9(09)V99 COMP-3.
           03  W40-A-CUR           PIC S9(09)V99 COMP-3.
           03  W40-A-30            PIC S9(09)V99 COMP-3.
           03  W40-A-60            PIC S9(09)V99 COMP-3.
           03  W40-A-90            PIC S9(09)V99 COMP-3.
           03  W40-A-120           PIC S9(09)V99 COMP-3.
           03  W40-A-INT           PIC S9(09)V99 COMP-3.
           03  W40-D-MTD           PIC S9(09)V99 COMP-3.
           03  W40-D-CUR           PIC S9(09)V99 COMP-3.
           03  W40-D-30            PIC S9(09)V99 COMP-3.
           03  W40-D-60            PIC S9(09)V99 COMP-3.
           03  W40-D-90            PIC S9(09)V99 COMP-3.
           03  W40-D-120           PIC S9(09)V99 COMP-3.
           03  W40-D-INT           PIC S9(09)V99 COMP-3.
           03  W40-T-MTD           PIC S9(09)V99 COMP-3.
           03  W40-T-CUR           PIC S9(09)V99 COMP-3.
           03  W40-T-30            PIC S9(09)V99 COMP-3.
           03  W40-T-60            PIC S9(09)V99 COMP-3.
           03  W40-T-90            PIC S9(09)V99 COMP-3.
           03  W40-T-120           PIC S9(09)V99 COMP-3.
           03  W40-T-INT           PIC S9(09)V99 COMP-3.

       COPY "W40.WS".

       COPY "FUNCTION.WS".

      *COPY "W50.WS".

       01  W50-NAME.
           03  W50-DATA.
               05  W50-CCHAR       PIC  X(01)            OCCURS 80.
           03  W50-DATA-LEFT                             REDEFINES W50-DATA.
               05  W50-D1          PIC  X(01).
               05  W50-D79         PIC  X(79).
           03  W50-INDIVIDUAL                            REDEFINES W50-DATA.
               05  W50-SURNAME.
                   07  W50-SNENT   PIC  X(12).
                   07  FILLER      PIC  X(12).
               05  W50-FNAME.
                   07  W50-1NENT   PIC  X(12).
                   07  FILLER      PIC  X(12).
               05  W50-SNAME.
                   07  W50-2NENT   PIC  X(12).
                   07  FILLER      PIC  X(12).
               05  W50-INIT.
                   07  W50-I1      PIC  X(01).
                   07  W50-I7      PIC  X(07).
           03  W50-NAME                                  REDEFINES W50-DATA.
               05  W50-ENTRY.
                   07  W50-DENT.
                       09  W50-NKEY PIC  X(12).
               05  W50-CPY          PIC  X(30).
               05  FILLER           PIC  X(38).

       01  W75-KEYS.
           03  W75-S               PIC  9(02)    COMP-5.
           03  W75-S1              PIC  9(02)    COMP-5.
           03  W75-KEY             PIC  9(06)    COMP-5.
           03  W75-ALLOCATED       PIC S9(09)V99 COMP-3.
           03  W75-BALANCE         PIC S9(09)V99 COMP-3.
           03  W75-DNO             PIC  X(06)            OCCURS 18.

       COPY "W75.WS".

       01  W80-ACCOUNT.
           03  W80-ACNO            PIC  X(06).
      *    03  W80-TYPE            PIC  9(01).

       01  W85-PASS.
           03  W85-SUPER           PIC  X(06)            OCCURS 9.
           03  W85-ENTRY           PIC  9(02)    COMP-3.
           03  W85-MARG            PIC S9(02)V99.

       01  W95-STM.
           03  W95-COMP            PIC  X(40).

       01  W100-EDIT.
           03  W100-LBAL           PIC  Z(06)9.99-.
           03  W100-LARR           PIC  Z(06)9.99.
           03  W100-BAL            PIC  Z(08)9.99-.
           03  W100-CUR            PIC  Z(08)9.99-.
           03  W100-30             PIC  Z(08)9.99-.
           03  W100-60             PIC  Z(08)9.99-.
           03  W100-90             PIC  Z(08)9.99-.
           03  W100-120            PIC  Z(08)9.99-.
           03  W100-INT            PIC  Z(08)9.99.
           03  W100-CRL            PIC  Z(06)9.
           03  W100-KEY            PIC  Z(05)9.

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

       01  S06.
           03  LINE  5 COLUMN 37 VALUE "INTERNAL" FOREGROUND-COLOR Grey HIGHLIGHT.

       01  S-ART11.
           03  BACKGROUND-COLOR Green FOREGROUND-COLOR Grey.
               05  LINE 11 COLUMN 16 VALUE "                                                  ".
               05  LINE 12 COLUMN 16 VALUE " Does this A/C qualify for VAT article 11/1g:     ".
               05  LINE 13 COLUMN 16 VALUE "                                                  ".

       01  S-ACT-LUP.
           03  BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
               05  LINE 50 COLUMN  2 VALUE "F1"                                                                 FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN  4 VALUE   "=Help,".
               05          COLUMN 10 VALUE         "F2"                                                         FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 12 VALUE           " to do Account Lookup,".
               05          COLUMN 34 VALUE                                 "F3"                                 FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 36 VALUE                                   " allocate account No. ".
               05          COLUMN 58 VALUE                                                         "Esc"        FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 61 VALUE                                                            " to exit".
       
       01  S20.
           03  LINE  6 COLUMN 12 VALUE "Account Type".
           03          COLUMN 29 VALUE             "(".
           03          COLUMN 30 VALUE              "C"                              FOREGROUND-COLOR Brown HIGHLIGHT.
           03          COLUMN 31 VALUE               "ompany, ".
           03          COLUMN 39 VALUE                       "I"                     FOREGROUND-COLOR Brown HIGHLIGHT.
           03          COLUMN 40 VALUE                        "ndividual, i".
           03          COLUMN 52 VALUE                                    "N"        FOREGROUND-COLOR Brown HIGHLIGHT.
           03          COLUMN 53 VALUE                                     "ternal)".

       01  S21.
           03  LINE  6 COLUMN 26 PIC  X(01) USING WS-TYPE FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta AUTO.

       COPY "DEBTOR.CRT".

       01  S36.
           03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta PIC X(06) USING W80-ACNO AUTO.
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
       AA000                SECTION.
       AA00.
           IF LS0-DBLEV < 1
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
               GO TO AA49.
             PERFORM ZA000-INIT.

       AA05.
             PERFORM CB000.
           IF NOT(WS-ERROR = 9)
               DISPLAY "Generating Alternate Name Index" AT 5018 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
               PERFORM DEBTOR-NAME-INDEX
               PERFORM AY70 THRU AY999
               MOVE "P R E S S"      TO WS-DSP-MES
               PERFORM DISPLAY-MESSAGE
               GO TO AA05.
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
       AA50-LOOKUP     SECTION.
       AA50.
             PERFORM SAVE-SCREEN.
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

      *
      *    ****    R E A D   F I L E S   R O U T I N E S
      *
       AC000              SECTION.

       COPY "CONTROL.RD".

       COPY "DEBTOR.RD".

       COPY "PARAM.RD".

       COPY "SHARED.RD".

      *                                                                 
      *     R E W R I T E   &   W R I T E   F I L E S   R O U T I N E S 
      *                                                                 
       AD000             SECTION.

       COPY "CONTROL.WR".

       COPY "DEBALT.WR".

       COPY "DEBTOR.WR".

       COPY "PARAM.WR".

       COPY "APAC.HLP".

      *
      *       ****   *****   ***    ***   *   *  *****  ****   *   *
      *       *   *  *      *   *  *   *  *   *  *      *   *  *   *
      *       ****   ***    *      *   *  *   *  ***    ****    * *
      *       *   *  *      *   *  *   *   * *   *      *   *    *
      *       *   *  *****   ***    ***     *    *****  *   *    *
      *    ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      *          THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *    ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      *        ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *        ³ ORIGINAL ACTION (REC-TYPE)  ÄÄ  RECOVERY PROCESS ³
      *        ³ 0 = RECORD CHANGED          ÄÄ     (REWRITE)     ³
      *        ³ 1 = RECORD WAS ADDED        ÄÄ     (DELETE)      ³
      *        ³ 2 = RECORD WAS DELETED      ÄÄ     (WRITE)       ³
      *        ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AY000        SECTION.

       AY02.
             MOVE 02                 TO REC-FILE.
             MOVE ZERO               TO REC-KEY.
             MOVE WS-ACTION          TO REC-TYPE
           IF WS-ACTION = 0 OR 2
               PERFORM READ-DEBTOR-LOCK THRU READ-DEBTOR-EXIT.
             MOVE DEB-RECORD1       TO REC-DEBTOR.
             GO TO AY50. 

       AY39.
             MOVE 39                 TO REC-FILE.
             MOVE WS-NETKEY          TO REC-KEY.
             MOVE NET-RECORD         TO REC-NETWORK.
             GO TO AY50.

       AY47.
             MOVE 47                 TO REC-FILE.
             MOVE ZERO               TO REC-KEY.
             MOVE WS-ACTION          TO REC-TYPE
             MOVE DAL-RECORD1        TO REC-DEBALT.
             GO TO AY50.

       AY49.
             MOVE 99                 TO REC-FILE.
             MOVE ZERO               TO REC-KEY.
             MOVE SPACES             TO REC-DETAIL.
             PERFORM AY50 THRU AY59.
             ADD 1                   TO WS-TRANS.
           IF WS-RECOVER > 95
               CLOSE RECOVER
               OPEN OUTPUT RECOVER
               CLOSE RECOVER
               OPEN I-O RECOVER
               MOVE ZERO             TO WS-RECOVER.
             GO TO AY59.

       AY50.
             ADD 1                   TO WS-RECOVER.
             MOVE WS-RECOVER         TO WS-RECKEY.
             MOVE WS-TRANS           TO REC-TRAN.
             WRITE REC-RECORD.
           IF WS-STATUS NOT = "00"
               MOVE SPACES           TO WS-ERR-MES
               STRING "Write error Recovery file - Status " DELIMITED SIZE WS-STATUS DELIMITED SIZE INTO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               STOP RUN.
       AY59.
             EXIT.
      *
      *    ****    Start processing transaction
      *
       AY60.
             MOVE 1                  TO WS-COUNT.
             MOVE 5                  TO WS-SHARED.
             PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT.
      *      MOVE SHR-STOCK          TO WS-STOCK.
      *
      *    ****    Read PARAM record 4 - Lock record and start recovery
      *            file procedure.
      *
             MOVE 4                  TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
           IF PAR-USERS < 24
               MOVE 1                TO WS-SUB
               MOVE ZERO             TO WS-WAIT
               GO TO AY62.
      *
      *    ****   Q   F U L L  -  W A I T   F O R   4 S E C O N D S
      *
             DISPLAY "WAITING" AT 5051 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Red HIGHLIGHT.
             COMMIT.
             ACCEPT WS-STIME FROM TIME.
             MOVE 400                TO WS-WAIT.
             PERFORM LOCK-REC-LOOP.
             DISPLAY SPACE AT 5051 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Cyan.
             GO TO AY60.

       AY61.
             MOVE "DB"               TO PAR-PROG(WS-USUB).
             MOVE LS-USER            TO PAR-USR(WS-USUB).
             PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      *
      *    ****    Read the DEBTOR control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *
             MOVE 1                  TO WS-NETKEY.
             PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
             PERFORM AY39 THRU AY59.
             GO TO AY999.
      *
      *    ****   A R E   A N Y   U P D A T E S   I N   P R O G R E S S
      *
       AY62.
           IF NOT(PAR-PROG(WS-SUB) = SPACES OR PAR-USR(WS-SUB) = SPACES)
      *
      *    ****   A R E   D E B T O R   O R   S T O C K   F I L E S
      *                     B E I N G   U P D A T E D
      *
               IF NOT(PAR-PROG(WS-SUB) = SPACES)
                   IF PAR-PROG(WS-SUB) = "DB" OR "DS"
      *
      *    ****   Y E S   -   S E T   W A I T P E R I O D
      *
                       GO TO AY62-WAIT
                   ELSE
                       ADD 1         TO WS-SUB
                       GO TO AY62
                   END-IF
               ELSE
      *
      *    ****   I S   T H I S   P R O G R A M   I N   T H E   Q
      *
               IF PAR-USR(WS-SUB) = LS-USER
      *
      *    ****   I S   I T   N E X T   I N   L I N E   T O   P R O C E S S
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
             MOVE LS-USER            TO PAR-USR(WS-SUB).
             MOVE WS-SUB             TO PAR-USERS.
             PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
             GO TO AY62-CHECK.
      *
      *    ****   S E T   W A I T   P E R I O D
      *
       AY62-WAIT.
             MOVE 300                TO WS-WAIT.
           IF NOT(PAR-USR(WS-SUB) = LS-USER)
               IF WS-SUB < 24
                   ADD 1             TO WS-SUB
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
      *
      *    ****    Write links back and unlock PARAM record 4 and
      *            NETWORK record 2.
      *
             PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
             PERFORM AY49 THRU AY59.
             MOVE 1                  TO WS-USUB.

       AY72.
           IF NOT(PAR-USR(WS-USUB) = LS-USER)
               IF WS-USUB < 24
                   ADD 1  TO WS-USUB
                   GO TO AY72
               ELSE
                   GO TO AY85.

       AY75.
             MOVE SPACES             TO PAR-PROG(WS-USUB) PAR-USR(WS-USUB).
           IF WS-USUB < 24
               ADD 1 WS-USUB         GIVING WS-SUB
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
      *    ****   C R E A T E   N E W   D E B T O R   R E C O R D
      *
       CB000   SECTION 3.
       CB00.
             PERFORM HEADING-AND-CLEAR-SCREEN.      
             DISPLAY S20.
             MOVE SPACE              TO WS-TYPE.
       CB05.
             ACCEPT S21.
             CALL "CBL_TOUPPER" USING WS-TYPE BY VALUE WS-LENGTH RETURNING WS-STATUS.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY      MOVE 9         TO WS-ERROR
                                   GO TO CB999
                                   
                 WHEN OTHER        PERFORM AA900-ALARM
                                   GO TO CB05
               END-EVALUATE.
           IF WS-TYPE = SPACE
               MOVE 9                TO WS-ERROR
               GO TO CB999.
           IF NOT(WS-TYPE = "N" OR "C" OR "I")
               GO TO CB05.
      *
      *   Using Initialize so have commented the move spaces and zeroes
      *
             INITIALIZE W40-DEBTREC.
             MOVE "N"                TO WS-ASSIGN.
      *      MOVE SPACES             TO W40-HTEL W40-NAME1 W40-ADDRESS1 W40-TEL W40-PPOST W40-PADDRESS W80-ACNO
      *                                  W40-POST W40-CONAC W40-CONTACT  W40-TAX W40-FAX   W40-ID.
             MOVE "E"                TO W40-LANG W40-INV-TYPE.
             MOVE "Y"                TO W40-FIN  W40-LAB.
             MOVE "O"                TO W40-OI.
             MOVE "1"                TO W40-PRICE.
             MOVE 01                 TO W40-CAT.
             MOVE "N"                TO W40-EXEMPT W40-RESTRICT.
      *      MOVE ZERO               TO W40-TITLE W40-TERMS W40-CRL W40-DISC W40-SDISC W40-SMAN.
           IF WS-TYPE = "I"
               MOVE 1                TO W40-TYPE
           ELSE
           IF WS-TYPE = "C"
               MOVE 2                TO W40-TYPE
           ELSE
               MOVE 3                TO W40-TYPE.
             MOVE 6                  TO CRT-START CRT-END.
             PERFORM ERASE-SCREEN.
           IF WS-TYPE NOT = "I"
               GO TO CB15 .
             MOVE SPACES             TO W40-HTEL.
             DISPLAY S22.
             DISPLAY S25.
       CB08.
             DISPLAY S-ACT-LUP.
             ACCEPT S36 AT 0615.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    MOVE SPACES     TO W80-ACNO
                                 MOVE 9          TO WS-ERROR
                                 GO TO CB999
                                 
                 WHEN F1-KEY     PERFORM HELP-ROUTINE
                 
                 WHEN F2-KEY     MOVE SPACE      TO W40-LUP
                                 PERFORM AA50-LOOKUP
                                 
                 WHEN F3-KEY     MOVE "Y"        TO WS-ASSIGN
                                 PERFORM CLEAR-L50
                                 GO TO CB10
                                 
                 WHEN F9-KEY     MOVE "A"        TO W40-LUP
                                 PERFORM AA50-LOOKUP
                                 
                 WHEN OTHER      PERFORM AA900-ALARM
                                 GO TO CB08
               END-EVALUATE
               DISPLAY S36 AT 0615
               IF W80-ACNO = SPACES
                   GO TO CB08.
           IF W80-ACNO = SPACES
                GO TO CB00.
             PERFORM CLEAR-L50.
             MOVE 6                  TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W80-ACNO BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 1                  TO WS-LENGTH.
             MOVE W80-ACNO           TO DEB-ACNO W40-ACNO.
             PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 0
               MOVE "Existing A/C number"  TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO CB08.

       CB10.
             ACCEPT S25.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY      MOVE "N"       TO WS-ASSIGN
                                   GO TO CB08
               END-EVALUATE.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               IF WS-ASSIGN = "N"
                   GO TO CB08
               ELSE
                   GO TO CB10.
           IF W40-SURNAME = SPACE
               MOVE 9                TO WS-ERROR
               GO TO CB999.
           IF  W40-ADD1 = SPACES OR
                W40-POST = SPACES OR W40-TITLE = ZERO
                MOVE WS-ERR3         TO WS-ERR-MES
                GO TO CB14.
             MOVE 6                  TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W40-CONAC    BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 8                  TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W40-POST     BY VALUE WS-LENGTH RETURNING WS-STATUS.
             CALL "CBL_TOUPPER" USING W40-PPOST    BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 1                  TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W40-FIN      BY VALUE WS-LENGTH RETURNING WS-STATUS.
             CALL "CBL_TOUPPER" USING W40-INV-TYPE BY VALUE WS-LENGTH RETURNING WS-STATUS.
             CALL "CBL_TOUPPER" USING W40-OI       BY VALUE WS-LENGTH RETURNING WS-STATUS.
             CALL "CBL_TOUPPER" USING W40-LAB      BY VALUE WS-LENGTH RETURNING WS-STATUS.
             CALL "CBL_TOUPPER" USING W40-PRICE    BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE "I"                TO W40-INSTR.
             CALL "DTP\DTPNAME" USING W40-COMPANY LS-USER-ID.
             DISPLAY S25-NAME.
             PERFORM CLEAR-L50.
             DISPLAY "Checking Input data" AT 5018 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
      *
      *    Format Addresses - Remove leading spaces
      *
             MOVE "F"                TO W40-INSTR.
             CALL "DTP\DTPADDR" USING W40-COMPANY.
             DISPLAY S25-ADDRESS.
           IF W40-TITLE < 0 OR > 10
               MOVE WS-ERR4          TO WS-ERR-MES
               GO TO CB14.
           IF W40-CAT < 1
               MOVE 1                TO W40-CAT.
           IF W40-CAT > 80
               MOVE WS-ERR8          TO WS-ERR-MES
               GO TO CB14.
           IF W40-TERMS > "3"
               MOVE "Terms"          TO WS-ERR-MES
               GO TO CB14.
           IF NOT(W40-FIN = "Y" OR "N")
               MOVE "Arrear Interest code" TO WS-ERR-MES
               GO TO CB14.
           IF W40-SMAN > 50
               MOVE "Salesperson"    TO WS-ERR-MES
               GO TO CB14.
           IF W40-DISC > 10 AND < 60
               MOVE "Discount"       TO WS-ERR-MES
               GO TO CB14.
           IF W40-SDISC > 10
               MOVE "Settlement Discount"  TO WS-ERR-MES
               GO TO CB14.
           IF NOT(W40-INV-TYPE = "I" OR "E" OR "X")
               MOVE "Invoice type"   TO WS-ERR-MES
               GO TO CB14.
           IF NOT(W40-OI = "O" OR "B")
               MOVE "Statement type" TO WS-ERR-MES
               GO TO CB14.
           IF NOT(W40-LAB = "Y" OR "N")
               MOVE "Label code"     TO WS-ERR-MES
               GO TO CB14.
           IF NOT(W40-PRICE = "1" OR "2" OR "3")
               MOVE "Price code"     TO WS-ERR-MES
               GO TO CB14.
           IF W40-CONAC NOT = SPACES
               MOVE W40-CONAC        TO DEB-ACNO
               PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT
               IF WS-F-ERROR = 6
                   MOVE "Consolidation Account"  TO WS-ERR-MES
                   GO TO CB14.
             GO TO CB24.

       CB14.
             PERFORM ERROR-MESSAGE.
             GO TO CB10.
      *
      *    ****   C O M P A N Y  /  I N T E R N A L   A / C
      *
       CB15.
             DISPLAY S23.
           IF WS-TYPE = "N"
               DISPLAY S06.
             DISPLAY S24.

       CB16.
             DISPLAY S-ACT-LUP.
             ACCEPT S36 AT 0615.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    MOVE SPACES    TO W80-ACNO
                                 MOVE 9         TO WS-ERROR
                                 GO TO CB999
                                 
                 WHEN F1-KEY     PERFORM HELP-ROUTINE
                 
                 WHEN F2-KEY     PERFORM AA50-LOOKUP
                 
                 WHEN F3-KEY     MOVE "Y"       TO WS-ASSIGN
                                 PERFORM CLEAR-L50
                                 GO TO CB20
                                 
                 WHEN OTHER      PERFORM AA900-ALARM
                                 GO TO CB16
                END-EVALUATE
                DISPLAY S36 AT 0615
                IF W80-ACNO = SPACES
                    GO TO CB16.
           IF W80-ACNO = SPACES
               GO TO CB00.
             PERFORM CLEAR-L50.
             MOVE 6                  TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W80-ACNO   BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 1                  TO WS-LENGTH.
             MOVE W80-ACNO           TO DEB-ACNO W40-ACNO.
             PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 0
               MOVE "Existing A/C number"  TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO CB16.
       CB20.
             ACCEPT S24.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    MOVE "N"            TO WS-ASSIGN
                                 GO TO CB16
               END-EVALUATE.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               IF WS-ASSIGN = "N"
                   GO TO CB16
               ELSE
                   GO TO CB20.
           IF W40-NAME = SPACE
               MOVE 9                TO WS-ERROR
               GO TO CB999.
             MOVE 6                  TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W40-CONAC    BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 8                  TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W40-POST     BY VALUE WS-LENGTH RETURNING WS-STATUS.
             CALL "CBL_TOUPPER" USING W40-PPOST    BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 1                  TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W40-FIN      BY VALUE WS-LENGTH RETURNING WS-STATUS.
             CALL "CBL_TOUPPER" USING W40-INV-TYPE BY VALUE WS-LENGTH RETURNING WS-STATUS.
             CALL "CBL_TOUPPER" USING W40-OI       BY VALUE WS-LENGTH RETURNING WS-STATUS.
             CALL "CBL_TOUPPER" USING W40-LAB      BY VALUE WS-LENGTH RETURNING WS-STATUS.
             CALL "CBL_TOUPPER" USING W40-PRICE    BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE "C"                TO W40-INSTR.
             CALL "DTP\DTPNAME" USING W40-COMPANY LS-USER-ID.
             DISPLAY S24-NAME.
             PERFORM CLEAR-L50.
             DISPLAY "Checking Input data" AT 5018 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
           IF W40-ADD1 = SPACES OR W40-POST = SPACES
               MOVE WS-ERR3          TO WS-ERR-MES
               GO TO CB23.
      *
      *    Format Addresses - Remove leading spaces
      *
             MOVE "F"                TO W40-INSTR.
             CALL "DTP\DTPADDR" USING W40-COMPANY.
             DISPLAY S24-ADDRESS.
           IF W40-CAT < 1
               MOVE 1                TO W40-CAT.
           IF W40-CAT > 80
               MOVE WS-ERR8          TO WS-ERR-MES
               GO TO CB23.
           IF NOT(W40-FIN = "Y" OR "N")
               MOVE "Arrear Interest code" TO WS-ERR-MES
               GO TO CB23.
           IF W40-TERMS < "0" OR > "3"
               MOVE "Terms"          TO WS-ERR-MES
               GO TO CB23.
           IF W40-SMAN > 50
               MOVE "Salesperson"    TO WS-ERR-MES
               GO TO CB23.
           IF W40-DISC > 10 AND < 60
               MOVE "Discount"       TO WS-ERR-MES
               GO TO CB23.
           IF W40-SDISC > 10
               MOVE "Settlement Discount" TO WS-ERR-MES
               GO TO CB23.
           IF NOT(W40-INV-TYPE = "I" OR "E" OR "X")
               MOVE "Invoice type"   TO WS-ERR-MES
               GO TO CB23.
           IF NOT(W40-OI = "O" OR "B")
               MOVE "Statement type" TO WS-ERR-MES
               GO TO CB23.
           IF NOT(W40-LAB = "Y" OR "N")
               MOVE "Label code"     TO WS-ERR-MES
               GO TO CB23.
           IF NOT(W40-PRICE = "1" OR "2" OR "3")
               MOVE "Price code"     TO WS-ERR-MES
               GO TO CB23.
           IF W40-CONAC NOT = SPACES
               MOVE W40-CONAC        TO DEB-ACNO
               PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT
               IF WS-F-ERROR = 6
                   MOVE "Consolidation Account"  TO WS-ERR-MES
                   GO TO CB23.
             GO TO CB24.

       CB23.
             PERFORM ERROR-MESSAGE.
             GO TO CB20.

       CB24.
             DISPLAY "Generating Account" AT 5018 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT.
           IF W40-ART11 = "N"
               GO TO CB25.

       CB24-ART11.
             PERFORM SAVE-SCREEN-3.
             MOVE 12                 TO SHADE-ROW.
             MOVE 18                 TO SHADE-COL.
             MOVE 48                 TO SHADE-WIDTH.
             MOVE 2                  TO SHADE-LINES.
             DISPLAY S-ART11.
             PERFORM SCREEN-SHADOW.

       CB24-EXEMPT.
             ACCEPT W40-EXEMPT AT 1263 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta UPDATE AUTO.
             CALL "CBL_TOUPPER" USING W40-EXEMPT BY VALUE WS-LENGTH RETURNING WS-STATUS.
           IF NOT(W40-EXEMPT = "N" OR "Y")
               GO TO CB24-EXEMPT.
             PERFORM RESTORE-SCREEN-3.

       CB25.
             PERFORM AY60 THRU AY999.

       CB25A.
           IF WS-ASSIGN = "Y"
               IF DEB-AC-NO = ZERO
                   ADD 1             TO DEB-AC-NO
               END-IF
               MOVE DEB-AC-NO        TO DEB-ACNO
               PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT
               IF WS-F-ERROR = 0
                   IF DEB-AC-NO = 999999
                       MOVE 1        TO DEB-AC-NO
                       GO TO CB25A
                   ELSE
                       ADD 1         TO DEB-AC-NO
                       GO TO CB25A
                   END-IF
               ELSE
                   MOVE DEB-ACNO     TO W80-ACNO
                   DISPLAY S36 AT 0615.
           IF WS-TYPE = "I"
               MOVE 1                TO W40-TYPE
           ELSE
           IF WS-TYPE = "C"
               MOVE 2                TO W40-TYPE
           ELSE
               MOVE 3                TO W40-TYPE.
             MOVE W40-DEBTREC        TO DEB-RECORD1.
             PERFORM WRITE-DEBTOR THRU WRITE-DEBTOR-EXIT.
             MOVE 1                  TO WS-ACTION.
             PERFORM AY02 THRU AY59.
             ADD 1                   TO DEB-ACCOUNTS.

       CB999.
             EXIT.
      *
      *    ****  A L T E R N A T E   N A M E   I N D E X
      *

       COPY "DEBTOR.NIX".

      *    *************************************************************
      *                I N I T I A L I S E   P R O G R A M
      *    *************************************************************
       ZA000-INIT        SECTION 8.
       ZA000-OPEN.
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

       ZA00A-CONTINUE.
             MOVE 3                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
           IF NOT(PAR-ART11 = "Y" OR "N")
               MOVE "N"              TO PAR-ART11.
             MOVE PAR-ART11          TO W40-ART11.
             MOVE 8                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-WS-HD          TO WS-WS-HD.
             MOVE PAR-CS-HD          TO WS-CS-HD.
             MOVE PAR-RT-HD          TO WS-RT-HD.
             OPEN OUTPUT RECOVER.
             CLOSE RECOVER.
             OPEN I-O RECOVER.
             MOVE 1                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-COMPANY        TO W95-COMP.
             MOVE PAR-DMY            TO W12-TODAY.
             MOVE PAR-YMD            TO W12-T-YMD.
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
               WHEN 2          MOVE W02-NETWORK  TO WS-FILE
                               MOVE WS-NETKEY    TO WS-KEY
                               
               WHEN 6          MOVE W02-DEBTOR   TO WS-FILE
                               MOVE DEB-ACNO     TO WS-KEYX
                               
               WHEN 15         MOVE WS-PARID     TO WS-FILE
                               MOVE WS-PARKEY    TO WS-KEY
                               
               WHEN 18         MOVE W02-RECOVER  TO WS-FILE
                               MOVE WS-RECKEY    TO WS-KEY
                               
               WHEN 37         MOVE W02-SHARED   TO WS-FILE
                               MOVE WS-SHARED    TO WS-KEY
                               
               WHEN 47         MOVE W02-DEBALT   TO WS-FILE
                               MOVE DAL-KEY      TO WS-KEYX
                               
               WHEN OTHER      MOVE "ERROR"      TO WS-FILE
                               MOVE "DTP001"     TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
